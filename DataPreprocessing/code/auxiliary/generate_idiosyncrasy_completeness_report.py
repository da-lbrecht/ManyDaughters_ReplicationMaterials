#!/usr/bin/env python3
"""
Generate idiosyncrasy completeness outputs:
1) HTML report: data/derived/idiosyncrasy_completeness_report.html
2) Flattened CSV export: data/derived/idiosyncrasy_completeness_missing_iteration_diagnostics.csv
"""

from __future__ import annotations

import argparse
import csv
import datetime as dt
import html
import re
from collections import Counter, defaultdict
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, List, Optional, Set, Tuple


EXCLUDED_RT_IDS = {
    "01iw87",
    "015w1n",
    "01vtfa",
    "016nbt",
    "01xrXX",
    "01f8uy",
    "01ygl2",
}

EXPECTED_ITERATIONS = set(range(1, 1001))
HYPOTHESES = (1, 2, 3, 4)

MANUAL_COMMENTS_FILENAME = "idiosyncrasy_completeness_manual_comments.csv"
MISSING_ITERATIONS_EXPORT_FILENAME = "idiosyncrasy_completeness_missing_iteration_diagnostics.csv"

RESULT_RE = re.compile(
    r"idiosyncrasy_(\d{8})_(\d{6})_([0-9A-Za-z]+)_h([1-4])_results_iter(\d+)\.csv$"
)
LOG_RE = re.compile(
    r"idiosyncrasy_(\d{8})_(\d{6})_([0-9A-Za-z]+)_h([1-4])_iter(\d+)\.log$"
)

ERROR_PATTERNS: List[Tuple[re.Pattern[str], str]] = [
    (re.compile(r"no observations", re.IGNORECASE), "no observations"),
    (re.compile(r"variable .* not found|not found", re.IGNORECASE), "variable not found"),
    (re.compile(r"file .* could not be opened|file .* not found", re.IGNORECASE), "file could not be opened"),
    (re.compile(r"invalid syntax", re.IGNORECASE), "invalid syntax"),
    (re.compile(r"type mismatch", re.IGNORECASE), "type mismatch"),
    (re.compile(r"conformability error", re.IGNORECASE), "conformability error"),
    (re.compile(r"insufficient observations", re.IGNORECASE), "insufficient observations"),
    (re.compile(r"assertion is false", re.IGNORECASE), "assertion failed"),
]
RETURN_CODE_RE = re.compile(r"\br\((\d+)\)")
RETURN_CODE_FLEX_RE = re.compile(r"\br\s*\(?\s*(\d+)\s*\)?", re.IGNORECASE)


@dataclass
class IterRecord:
    rt_id: str
    hypothesis: int
    iteration: int
    timestamp: dt.datetime
    path: Path


@dataclass
class CellSummary:
    rt_id: str
    hypothesis: int
    observed_count: int
    missing_iterations: List[int]
    reason_category: str
    reason_detail: str
    reason_detail_lines: List[str]
    per_iteration_signatures: Dict[int, str]
    latest_result_ts: Optional[dt.datetime]
    latest_log_ts: Optional[dt.datetime]
    confidence: str


def parse_ts(date_str: str, time_str: str) -> Optional[dt.datetime]:
    try:
        return dt.datetime.strptime(f"{date_str}{time_str}", "%Y%m%d%H%M%S")
    except ValueError:
        return None


def escape(text: str) -> str:
    return html.escape(text, quote=True)


def fmt_ts(ts: Optional[dt.datetime]) -> str:
    if ts is None:
        return "none"
    return ts.strftime("%Y-%m-%d %H:%M:%S")


def parse_hypothesis_token(raw: str) -> Optional[int]:
    token = raw.strip().lower()
    if token.startswith("h"):
        token = token[1:]
    try:
        hyp = int(token)
    except ValueError:
        return None
    if hyp not in HYPOTHESES:
        return None
    return hyp


def discover_records(root: Path, regex: re.Pattern[str]) -> List[IterRecord]:
    records: List[IterRecord] = []
    if not root.exists():
        return records

    for path in root.rglob("*"):
        if not path.is_file():
            continue
        m = regex.match(path.name)
        if not m:
            continue

        date_str, time_str, rt_id, hyp_str, iter_str = m.groups()
        if rt_id in EXCLUDED_RT_IDS:
            continue

        ts = parse_ts(date_str, time_str)
        if ts is None:
            continue

        records.append(
            IterRecord(
                rt_id=rt_id,
                hypothesis=int(hyp_str),
                iteration=int(iter_str),
                timestamp=ts,
                path=path,
            )
        )
    return records


def get_latest_completeness_log(check_log_dir: Path, project_root: Path) -> str:
    if not check_log_dir.exists():
        return "none"
    logs = sorted(check_log_dir.glob("*.log"), key=lambda p: p.stat().st_mtime, reverse=True)
    if not logs:
        return "none"
    return str(logs[0].relative_to(project_root)).replace("\\", "/")


def infer_error_signature(log_path: Path, tail_lines: int = 120) -> Optional[str]:
    try:
        with log_path.open("r", encoding="utf-8", errors="replace") as f:
            lines = f.readlines()
    except OSError:
        return None

    tail = "\n".join(lines[-tail_lines:])
    m = RETURN_CODE_RE.search(tail)
    if m:
        return f"Stata return code r({m.group(1)})"

    for pattern, label in ERROR_PATTERNS:
        if pattern.search(tail):
            return label

    if "error" in tail.lower():
        return "generic error"

    return None


def infer_error_signature_cached(log_path: Path, cache: Dict[Path, Optional[str]]) -> Optional[str]:
    if log_path in cache:
        return cache[log_path]
    sig = infer_error_signature(log_path)
    cache[log_path] = sig
    return sig


def extract_return_code(text: str) -> Optional[str]:
    m = RETURN_CODE_FLEX_RE.search(text)
    if not m:
        return None
    return m.group(1)


def parse_code_specific_comment_fragments(comment: str) -> Dict[str, str]:
    """
    Parse a free-text cell-level comment into code-specific fragments.

    Example:
    "convergence not achieved (r430) or variance matrix nonsymmetric or highly singular r(111)"
    -> {"430": "convergence not achieved (r430)", "111": "variance matrix nonsymmetric or highly singular r(111)"}
    """
    matches = list(RETURN_CODE_FLEX_RE.finditer(comment))
    if not matches:
        return {}

    fragments: Dict[str, str] = {}
    start = 0
    for m in matches:
        code = m.group(1)
        fragment = comment[start : m.end()].strip(" ,;|/")
        fragment = re.sub(r"^(?:or|and)\s+", "", fragment, flags=re.IGNORECASE)
        if fragment:
            fragments[code] = fragment
        start = m.end()
    return fragments


def derive_iteration_comments_from_cell_comment(
    cell_comments_for_cell: List[str],
    auto_signature: str,
) -> List[str]:
    code = extract_return_code(auto_signature)
    if code is None:
        return []

    derived: List[str] = []
    seen: Set[str] = set()
    for comment in cell_comments_for_cell:
        by_code = parse_code_specific_comment_fragments(comment)
        specific = by_code.get(code)
        if specific and specific not in seen:
            seen.add(specific)
            derived.append(specific)
    return derived


def ensure_manual_comments_template(path: Path) -> None:
    if path.exists():
        return
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", newline="", encoding="utf-8") as f:
        writer = csv.writer(f)
        writer.writerow(["rt_id", "hypothesis", "iteration", "comment"])


def load_manual_comments_for_html(path: Path) -> Dict[Tuple[str, int], List[str]]:
    comments: Dict[Tuple[str, int], List[Tuple[int, str]]] = defaultdict(list)
    if not path.exists():
        return {}

    # utf-8-sig also accepts plain UTF-8 and strips an Excel-style BOM when present.
    with path.open("r", newline="", encoding="utf-8-sig", errors="replace") as f:
        reader = csv.DictReader(f, skipinitialspace=True)
        required = {"rt_id", "hypothesis", "iteration", "comment"}
        if not required.issubset(set(reader.fieldnames or [])):
            return {}

        for row in reader:
            rt_id = (row.get("rt_id") or "").strip()
            hyp_raw = (row.get("hypothesis") or "").strip()
            iter_raw = (row.get("iteration") or "").strip()
            comment = (row.get("comment") or "").strip()
            # Recover comment fragments when CSV rows were split by embedded commas.
            extra_parts = row.get(None) or []
            if extra_parts:
                tail = ",".join(part.strip() for part in extra_parts if part is not None)
                if tail:
                    comment = f"{comment},{tail}" if comment else tail

            if len(comment) >= 2 and comment[0] == '"' and comment[-1] == '"':
                comment = comment[1:-1].strip()

            if not rt_id or not hyp_raw or not comment:
                continue
            if rt_id in EXCLUDED_RT_IDS:
                continue

            hyp = parse_hypothesis_token(hyp_raw)
            if hyp is None:
                continue

            if iter_raw == "":
                comments[(rt_id, hyp)].append((0, f"cell: {comment}"))
            else:
                try:
                    it = int(iter_raw)
                except ValueError:
                    continue
                comments[(rt_id, hyp)].append((it, f"iter {it}: {comment}"))

    out: Dict[Tuple[str, int], List[str]] = {}
    for key, items in comments.items():
        out[key] = [line for _, line in sorted(items, key=lambda x: (x[0], x[1]))]
    return out


def load_manual_comments_for_export(
    path: Path,
) -> Tuple[Dict[Tuple[str, int], List[str]], Dict[Tuple[str, int, int], List[str]]]:
    cell_comments: Dict[Tuple[str, int], List[str]] = defaultdict(list)
    iter_comments: Dict[Tuple[str, int, int], List[str]] = defaultdict(list)

    if not path.exists():
        return {}, {}

    # utf-8-sig also accepts plain UTF-8 and strips an Excel-style BOM when present.
    with path.open("r", newline="", encoding="utf-8-sig", errors="replace") as f:
        reader = csv.DictReader(f, skipinitialspace=True)
        required = {"rt_id", "hypothesis", "iteration", "comment"}
        if not required.issubset(set(reader.fieldnames or [])):
            return {}, {}

        for row in reader:
            rt_id = (row.get("rt_id") or "").strip()
            hyp_raw = (row.get("hypothesis") or "").strip()
            iter_raw = (row.get("iteration") or "").strip()
            comment = (row.get("comment") or "").strip()
            # Recover comment fragments when CSV rows were split by embedded commas.
            extra_parts = row.get(None) or []
            if extra_parts:
                tail = ",".join(part.strip() for part in extra_parts if part is not None)
                if tail:
                    comment = f"{comment},{tail}" if comment else tail

            if len(comment) >= 2 and comment[0] == '"' and comment[-1] == '"':
                comment = comment[1:-1].strip()

            if not rt_id or not hyp_raw or not comment:
                continue
            if rt_id in EXCLUDED_RT_IDS:
                continue

            hyp = parse_hypothesis_token(hyp_raw)
            if hyp is None:
                continue

            if iter_raw == "":
                cell_comments[(rt_id, hyp)].append(comment)
            else:
                try:
                    it = int(iter_raw)
                except ValueError:
                    continue
                iter_comments[(rt_id, hyp, it)].append(comment)

    return dict(cell_comments), dict(iter_comments)


def classify_cell(
    rt_id: str,
    hypothesis: int,
    observed_iters: Set[int],
    log_records_by_iter: Dict[int, IterRecord],
    latest_result_ts: Optional[dt.datetime],
    latest_log_ts: Optional[dt.datetime],
    error_counter: Counter,
    signature_cache: Dict[Path, Optional[str]],
) -> CellSummary:
    observed_domain = observed_iters & EXPECTED_ITERATIONS
    missing = sorted(EXPECTED_ITERATIONS - observed_domain)

    if not missing:
        return CellSummary(
            rt_id=rt_id,
            hypothesis=hypothesis,
            observed_count=len(observed_domain),
            missing_iterations=[],
            reason_category="Complete",
            reason_detail="All expected iterations 1..1000 are present.",
            reason_detail_lines=[],
            per_iteration_signatures={},
            latest_result_ts=latest_result_ts,
            latest_log_ts=latest_log_ts,
            confidence="high",
        )

    missing_logs = [it for it in missing if it not in log_records_by_iter]
    reason_detail_lines: List[str] = []
    per_iter_sig: Dict[int, str] = {}
    sigs: List[str] = []

    for it in missing:
        rec = log_records_by_iter.get(it)
        if rec is None:
            reason_detail_lines.append(f"iter {it}: no log file found")
            per_iter_sig[it] = "no log file found"
            continue

        sig = infer_error_signature_cached(rec.path, signature_cache)
        if sig:
            reason_detail_lines.append(f"iter {it}: {sig} [{rec.path.name}]")
            per_iter_sig[it] = sig
            sigs.append(sig)
        else:
            reason_detail_lines.append(f"iter {it}: no explicit error signature [{rec.path.name}]")
            per_iter_sig[it] = "no explicit error signature"

    if len(missing_logs) == len(missing):
        reason = "No run evidence"
        detail = "No matching log files found for missing iterations."
        confidence = "high"
    elif sigs:
        reason = "Error found in log tail"
        top_sig, top_count = Counter(sigs).most_common(1)[0]
        detail = f"Top signature among missing-iteration logs: {top_sig} ({top_count} log(s))."
        confidence = "high"
        error_counter[top_sig] += 1
    elif missing_logs:
        reason = "Partial artifact mismatch"
        detail = "Some missing iterations have logs but others have no log evidence; no explicit error signature detected in available tails."
        confidence = "medium"
    else:
        reason = "Run attempted, no clear error"
        detail = "Missing iterations have logs, but no explicit error signature was detected in log tails."
        confidence = "medium"

    return CellSummary(
        rt_id=rt_id,
        hypothesis=hypothesis,
        observed_count=len(observed_domain),
        missing_iterations=missing,
        reason_category=reason,
        reason_detail=detail,
        reason_detail_lines=reason_detail_lines,
        per_iteration_signatures=per_iter_sig,
        latest_result_ts=latest_result_ts,
        latest_log_ts=latest_log_ts,
        confidence=confidence,
    )


def write_missing_iteration_export(
    csv_path: Path,
    incomplete_summaries: List[CellSummary],
    logs_by_cell_iter: Dict[Tuple[str, int], Dict[int, IterRecord]],
    cell_comments: Dict[Tuple[str, int], List[str]],
    iter_comments: Dict[Tuple[str, int, int], List[str]],
) -> None:
    csv_path.parent.mkdir(parents=True, exist_ok=True)
    with csv_path.open("w", newline="", encoding="utf-8") as f:
        writer = csv.writer(f)
        writer.writerow(
            [
                "rt_id",
                "hypothesis",
                "missing_iteration",
                "observed_count",
                "missing_count",
                "reason_category",
                "auto_signature",
                "log_exists",
                "log_filename",
                "log_timestamp",
                "latest_result_ts",
                "latest_log_ts",
                "cell_reason_summary",
                "manual_comment_iteration",
                "manual_comment_cell",
            ]
        )

        for s in sorted(incomplete_summaries, key=lambda x: (x.rt_id, x.hypothesis)):
            key = (s.rt_id, s.hypothesis)
            iter_map = logs_by_cell_iter.get(key, {})
            cell_comments_for_cell = cell_comments.get(key, [])
            cell_comment = " | ".join(cell_comments_for_cell)
            missing_count = len(s.missing_iterations)

            for missing_it in s.missing_iterations:
                rec = iter_map.get(missing_it)
                iter_key = (s.rt_id, s.hypothesis, missing_it)
                explicit_iter_comments = iter_comments.get(iter_key, [])

                iter_comment_parts: List[str] = list(explicit_iter_comments)
                if not iter_comment_parts:
                    auto_sig = s.per_iteration_signatures.get(missing_it, "")
                    iter_comment_parts.extend(
                        derive_iteration_comments_from_cell_comment(
                            cell_comments_for_cell=cell_comments_for_cell,
                            auto_signature=auto_sig,
                        )
                    )

                iter_comment = " | ".join(iter_comment_parts)
                writer.writerow(
                    [
                        s.rt_id,
                        s.hypothesis,
                        missing_it,
                        s.observed_count,
                        missing_count,
                        s.reason_category,
                        s.per_iteration_signatures.get(missing_it, ""),
                        "yes" if rec else "no",
                        rec.path.name if rec else "",
                        fmt_ts(rec.timestamp) if rec else "",
                        fmt_ts(s.latest_result_ts),
                        fmt_ts(s.latest_log_ts),
                        s.reason_detail,
                        iter_comment,
                        cell_comment,
                    ]
                )


def build_report(project_root: Path, output_path: Path, missing_export_path: Path) -> Path:
    results_dir = project_root / "data" / "derived" / "5_idiosyncrasy_results"
    logs_dir = project_root / "data" / "derived" / "5_idiosyncrasy_logs"
    check_log_dir = project_root / "logs" / "5_idiosyncrasy_check_logs"
    comments_path = project_root / "data" / "derived" / MANUAL_COMMENTS_FILENAME

    ensure_manual_comments_template(comments_path)
    manual_comments_html = load_manual_comments_for_html(comments_path)
    manual_cell_comments, manual_iter_comments = load_manual_comments_for_export(comments_path)

    result_records = discover_records(results_dir, RESULT_RE)
    log_records = discover_records(logs_dir, LOG_RE)

    rt_ids: Set[str] = set()
    rt_ids.update(r.rt_id for r in result_records)
    rt_ids.update(r.rt_id for r in log_records)

    if results_dir.exists():
        for p in results_dir.iterdir():
            if p.is_dir() and p.name not in EXCLUDED_RT_IDS:
                rt_ids.add(p.name)
    if logs_dir.exists():
        for p in logs_dir.iterdir():
            if p.is_dir() and p.name not in EXCLUDED_RT_IDS:
                rt_ids.add(p.name)

    observed_iters_by_cell: Dict[Tuple[str, int], Set[int]] = defaultdict(set)
    latest_result_ts_by_cell: Dict[Tuple[str, int], dt.datetime] = {}
    for rec in result_records:
        key = (rec.rt_id, rec.hypothesis)
        observed_iters_by_cell[key].add(rec.iteration)
        prev = latest_result_ts_by_cell.get(key)
        if prev is None or rec.timestamp > prev:
            latest_result_ts_by_cell[key] = rec.timestamp

    logs_by_cell_iter: Dict[Tuple[str, int], Dict[int, IterRecord]] = defaultdict(dict)
    latest_log_ts_by_cell: Dict[Tuple[str, int], dt.datetime] = {}
    for rec in log_records:
        key = (rec.rt_id, rec.hypothesis)
        prev = logs_by_cell_iter[key].get(rec.iteration)
        if prev is None or rec.timestamp > prev.timestamp:
            logs_by_cell_iter[key][rec.iteration] = rec
        prev_ts = latest_log_ts_by_cell.get(key)
        if prev_ts is None or rec.timestamp > prev_ts:
            latest_log_ts_by_cell[key] = rec.timestamp

    cells = [(rt_id, hyp) for rt_id in sorted(rt_ids) for hyp in HYPOTHESES]

    error_counter: Counter = Counter()
    signature_cache: Dict[Path, Optional[str]] = {}
    all_summaries: List[CellSummary] = []
    incomplete: List[CellSummary] = []

    for rt_id, hyp in cells:
        summary = classify_cell(
            rt_id=rt_id,
            hypothesis=hyp,
            observed_iters=observed_iters_by_cell.get((rt_id, hyp), set()),
            log_records_by_iter=logs_by_cell_iter.get((rt_id, hyp), {}),
            latest_result_ts=latest_result_ts_by_cell.get((rt_id, hyp)),
            latest_log_ts=latest_log_ts_by_cell.get((rt_id, hyp)),
            error_counter=error_counter,
            signature_cache=signature_cache,
        )
        all_summaries.append(summary)
        if summary.reason_category != "Complete":
            incomplete.append(summary)

    total_cells = len(all_summaries)
    complete_cells = total_cells - len(incomplete)
    incomplete_cells = len(incomplete)
    completion_rate = (complete_cells / total_cells * 100.0) if total_cells else 0.0

    write_missing_iteration_export(
        csv_path=missing_export_path,
        incomplete_summaries=incomplete,
        logs_by_cell_iter=logs_by_cell_iter,
        cell_comments=manual_cell_comments,
        iter_comments=manual_iter_comments,
    )

    manual_error_counts = Counter(
        comment
        for comments in (*manual_cell_comments.values(), *manual_iter_comments.values())
        for comment in comments
    )
    manual_error_rows = [
        f"<tr><td>{escape(comment)}</td><td>{count}</td></tr>"
        for comment, count in sorted(
            manual_error_counts.items(), key=lambda item: (-item[1], item[0].casefold())
        )
    ]
    if not manual_error_rows:
        manual_error_rows.append(
            '<tr><td colspan="2" class="muted">No errors recorded in manual comments.</td></tr>'
        )
    manual_error_rows.append(
        f'<tr><td><strong>Total</strong></td><td><strong>{sum(manual_error_counts.values())}</strong></td></tr>'
    )

    row_html: List[str] = []
    for s in sorted(incomplete, key=lambda x: (x.rt_id, x.hypothesis)):
        missing_count = len(s.missing_iterations)
        missing_preview = (
            "1, 2, 3, ... , 998, 999, 1000"
            if missing_count == 1000
            else ", ".join(str(i) for i in s.missing_iterations)
        )

        reason_badge_cls = {
            "No run evidence": "badge-none",
            "Error found in log tail": "badge-err",
            "Run attempted, no clear error": "badge-warn",
            "Partial artifact mismatch": "badge-warn",
        }.get(s.reason_category, "badge-warn")

        manual_lines = manual_comments_html.get((s.rt_id, s.hypothesis), [])
        if manual_lines:
            manual_block = (
                "    <details>\n"
                f"      <summary>Show notes ({len(manual_lines)})</summary>\n"
                f"      <pre>{escape(chr(10).join(manual_lines))}</pre>\n"
                "    </details>"
            )
        else:
            manual_block = "    <span class=\"muted\">none</span>"

        row_html.append(
            "\n".join(
                [
                    "<tr>",
                    f"  <td>{escape(s.rt_id)}</td>",
                    f"  <td>H{s.hypothesis}</td>",
                    f"  <td>{s.observed_count}</td>",
                    f"  <td>{missing_count}</td>",
                    "  <td>",
                    "    <details>",
                    f"      <summary>Show list ({missing_count})</summary>",
                    f"      <pre>{escape(missing_preview)}</pre>",
                    "    </details>",
                    "  </td>",
                    f"  <td><span class=\"badge {reason_badge_cls}\">{escape(s.reason_category)}</span></td>",
                    "  <td>",
                    "    <details>",
                    f"      <summary>{escape(s.reason_detail)} Show details ({missing_count})</summary>",
                    f"      <pre>{escape(chr(10).join(s.reason_detail_lines))}</pre>",
                    "    </details>",
                    "  </td>",
                    "  <td>",
                    manual_block,
                    "  </td>",
                    f"  <td>{escape(fmt_ts(s.latest_result_ts))}</td>",
                    f"  <td>{escape(fmt_ts(s.latest_log_ts))}</td>",
                    "</tr>",
                ]
            )
        )

    generated = dt.datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    latest_check_log_rel = get_latest_completeness_log(check_log_dir, project_root)

    html_doc = f"""<!doctype html>
<html lang=\"en\">
<head>
  <meta charset=\"utf-8\" />
  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\" />
  <title>Idiosyncrasy Completeness Report</title>
  <style>
    :root {{
      --bg: #f7f8fa;
      --card: #ffffff;
      --text: #1f2937;
      --muted: #64748b;
      --border: #dbe2ea;
      --ok: #1a7f37;
      --warn: #b45309;
      --err: #b42318;
      --none: #475467;
      --accent: #0f4c81;
      --shadow: 0 2px 12px rgba(15, 23, 42, 0.06);
    }}
    * {{ box-sizing: border-box; }}
    body {{
      margin: 0;
      font-family: \"Segoe UI\", \"Noto Sans\", \"Helvetica Neue\", sans-serif;
      color: var(--text);
      background:
        radial-gradient(circle at 5% 0%, #eef4fb 0, transparent 30%),
        radial-gradient(circle at 95% 0%, #edf8f3 0, transparent 32%),
        var(--bg);
      line-height: 1.4;
    }}
    .container {{ max-width: 1300px; margin: 0 auto; padding: 20px; }}
    .header {{ background: linear-gradient(120deg, #0f4c81, #136f63); color: #fff; border-radius: 14px; padding: 20px; box-shadow: var(--shadow); }}
    .header h1 {{ margin: 0; font-size: 1.6rem; }}
    .header-meta {{ margin-top: 10px; display: grid; grid-template-columns: repeat(auto-fit, minmax(240px, 1fr)); gap: 8px; font-size: 0.93rem; opacity: 0.95; }}
    .header-meta > div {{ min-width: 0; overflow-wrap: anywhere; word-break: break-word; }}
    .section {{ margin-top: 16px; background: var(--card); border: 1px solid var(--border); border-radius: 14px; box-shadow: var(--shadow); overflow: hidden; }}
    .section-title {{ margin: 0; padding: 14px 16px; font-size: 1.02rem; border-bottom: 1px solid var(--border); background: #fbfcfe; }}
    .section-body {{ padding: 14px 16px 16px; }}
    .cards {{ display: grid; grid-template-columns: repeat(auto-fit, minmax(170px, 1fr)); gap: 10px; }}
    .card {{ border: 1px solid var(--border); background: #fff; border-radius: 10px; padding: 12px; }}
    .card .label {{ color: var(--muted); font-size: 0.82rem; text-transform: uppercase; letter-spacing: 0.05em; }}
    .card .value {{ margin-top: 6px; font-size: 1.25rem; font-weight: 700; }}
    .row {{ display: flex; flex-wrap: wrap; gap: 8px; align-items: center; }}
    .pill {{ display: inline-flex; align-items: center; border: 1px solid var(--border); background: #fff; border-radius: 999px; padding: 5px 10px; font-size: 0.83rem; color: var(--text); gap: 8px; }}
    .k-ok {{ color: var(--ok); }}
    .k-warn {{ color: var(--warn); }}
    .k-err {{ color: var(--err); }}
    .k-none {{ color: var(--none); }}
    .table-wrap {{ overflow: auto; border: 1px solid var(--border); border-radius: 10px; }}
    table {{ width: 100%; border-collapse: collapse; min-width: 1200px; font-size: 0.9rem; }}
    thead th {{ position: sticky; top: 0; z-index: 1; text-align: left; background: #f5f8fc; border-bottom: 1px solid var(--border); padding: 10px; white-space: nowrap; }}
    tbody td {{ border-bottom: 1px solid #eef2f6; vertical-align: top; padding: 9px 10px; }}
    tbody tr:hover {{ background: #f9fbfd; }}
    .badge {{ display: inline-flex; align-items: center; gap: 5px; border-radius: 6px; padding: 3px 7px; font-size: 0.78rem; font-weight: 600; border: 1px solid transparent; }}
    .badge-warn {{ color: var(--warn); background: #fffaeb; border-color: #fedf89; }}
    .badge-err {{ color: var(--err); background: #fef3f2; border-color: #fecdca; }}
    .badge-none {{ color: var(--none); background: #f2f4f7; border-color: #d0d5dd; }}
    .muted {{ color: var(--muted); }}
    details {{ border: 1px dashed #cbd5e1; border-radius: 8px; padding: 6px 8px; background: #fbfdff; }}
    summary {{ cursor: pointer; font-weight: 600; color: var(--accent); }}
    pre {{ white-space: pre-wrap; margin: 8px 0 0; font-size: 0.8rem; color: #334155; }}
    .footnote {{ margin-top: 8px; color: var(--muted); font-size: 0.82rem; }}
    @media (max-width: 880px) {{
      .header h1 {{ font-size: 1.3rem; }}
      .section-title {{ font-size: 0.95rem; }}
      table {{ min-width: 980px; }}
    }}
  </style>
</head>
<body>
  <main class=\"container\">
    <header class=\"header\">
      <h1>Idiosyncrasy Completeness Report</h1>
      <div class=\"header-meta\">
        <div><strong>Generated:</strong> {escape(generated)}</div>
        <div><strong>Completeness Log:</strong> {escape(latest_check_log_rel)}</div>
        <div><strong>Manual Comments CSV:</strong> data/derived/{escape(MANUAL_COMMENTS_FILENAME)}</div>
        <div><strong>Missing Iterations CSV Export:</strong> data/derived/{escape(MISSING_ITERATIONS_EXPORT_FILENAME)}</div>
        <div><strong>Result Files Dir:</strong> data/derived/5_idiosyncrasy_results</div>
        <div><strong>Iteration Log Dir:</strong> data/derived/5_idiosyncrasy_logs</div>
      </div>
    </header>

    <section class=\"section\">
      <h2 class=\"section-title\">Summary</h2>
      <div class=\"section-body\">
        <div class=\"cards\">
          <article class=\"card\"><div class=\"label\">Total Cells (rt_id x hypothesis)</div><div class=\"value\">{total_cells}</div></article>
          <article class=\"card\"><div class=\"label\">Complete Cells (1000/1000)</div><div class=\"value k-ok\">{complete_cells}</div></article>
          <article class=\"card\"><div class=\"label\">Incomplete Cells</div><div class=\"value k-warn\">{incomplete_cells}</div></article>
          <article class=\"card\"><div class=\"label\">Completion Rate</div><div class=\"value\">{completion_rate:.1f}%</div></article>
        </div>
        <h3>Errors encountered in incomplete cells</h3>
        <div class="table-wrap">
          <table style="min-width: 500px;">
            <thead><tr><th>Error description based on manual inspection of log-files of incomplete cells</th><th>Occurrences</th></tr></thead>
            <tbody>{"".join(manual_error_rows)}</tbody>
          </table>
        </div>
      </div>
    </section>

    <section class=\"section\">
      <h2 class=\"section-title\">Incomplete Cell Master Table</h2>
      <div class=\"section-body\">
        <div class=\"table-wrap\">
          <table>
            <thead>
              <tr>
                <th>rt_id</th>
                <th>Hyp</th>
                <th>Observed</th>
                <th>Missing #</th>
                <th>Missing Iterations</th>
                <th>Reason Category</th>
                <th>Reason Detail</th>
                <th>Manual Review Comment</th>
                <th>Latest Result TS</th>
                <th>Latest Log TS</th>
              </tr>
            </thead>
            <tbody>
              {"".join(row_html) if row_html else '<tr><td colspan="10" class="muted">No incomplete cells.</td></tr>'}
            </tbody>
          </table>
        </div>
      </div>
    </section>

    <section class=\"section\">
      <h2 class=\"section-title\">Appendix: Parsing Rules and Assumptions</h2>
      <div class=\"section-body\">
        <ul>
          <li>Expected iteration domain per cell: 1..1000.</li>
          <li>Cell key: (rt_id, hypothesis).</li>
          <li>Excluded known inconsistent rt_id values: 01iw87, 015w1n, 01vtfa, 016nbt, 01xrXX, 01f8uy, 01ygl2.</li>
          <li>Result-file evidence comes from filenames in data/derived/5_idiosyncrasy_results.</li>
          <li>Run-evidence comes from filenames and log-tail parsing in data/derived/5_idiosyncrasy_logs.</li>
          <li>Optional manual notes are loaded from data/derived/{escape(MANUAL_COMMENTS_FILENAME)}.</li>
        </ul>
      </div>
    </section>
  </main>
</body>
</html>
"""

    output_path.parent.mkdir(parents=True, exist_ok=True)
    output_path.write_text(html_doc, encoding="utf-8")
    return output_path


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Generate idiosyncrasy completeness report and exports")
    parser.add_argument(
        "--project-root",
        type=Path,
        default=Path(__file__).resolve().parents[2],
        help="Project root path (default: inferred from script location)",
    )
    parser.add_argument(
        "--output",
        type=Path,
        default=None,
        help="Output HTML path (default: <project-root>/data/derived/idiosyncrasy_completeness_report.html)",
    )
    parser.add_argument(
        "--missing-iterations-export",
        type=Path,
        default=None,
        help=(
            "Flattened CSV export path "
            "(default: <project-root>/data/derived/idiosyncrasy_completeness_missing_iteration_diagnostics.csv)"
        ),
    )
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    project_root = args.project_root.resolve()

    html_output = args.output or (project_root / "data" / "derived" / "idiosyncrasy_completeness_report.html")
    html_output = html_output.resolve()

    missing_export = args.missing_iterations_export or (
        project_root / "data" / "derived" / MISSING_ITERATIONS_EXPORT_FILENAME
    )
    missing_export = missing_export.resolve()

    report_path = build_report(
        project_root=project_root,
        output_path=html_output,
        missing_export_path=missing_export,
    )

    print(f"Report written: {report_path}")
    print(f"Missing-iteration CSV written: {missing_export}")


if __name__ == "__main__":
    main()
