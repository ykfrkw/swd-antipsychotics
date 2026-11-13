#!/usr/bin/env python3
"""Build the manuscript Excel tables from the CSVs written by 03/04/05.

Copyright (c) 2026 the SWD study authors. Released under the MIT License (see LICENSE).
Author: SWD study authors (see LICENSE). Date: 2026-11-13 (last revised).

Purpose: Format the manuscript tables (Table 1-3, Appendix Tables S1-S2) as
    styled Excel workbooks: one combined workbook with five sheets and one
    standalone single-sheet workbook per table.
Inputs:  R_SWD/outputs/report/table1_country.csv, table2_swd_subgroups.csv,
    table3_swd_correlates.csv, table_s1_appendix.csv,
    table_s2_regression_sensitivity.csv
Outputs: R_SWD/outputs/report/table1_country.xlsx (combined workbook),
    table1_characteristics.xlsx, table2_swd_subgroups.xlsx,
    table3_swd_correlates.xlsx, table_s1_appendix.xlsx,
    table_s2_regression.xlsx
Depends: 03_table1_by_country.R, 04_table2_subgroups.R,
    05_table3_regression.R (run first); Python 3 with openpyxl
Style:   r_script_style_guide.md (Tier A principles)

Styling: Aptos 10 pt throughout; title row; bold variable/section rows;
level rows indented by four spaces; header row with a medium top border and
a thin bottom border; a medium border at the end of the table; value columns
centered; footnotes in italic 9 pt; frozen header row. Rows whose level is
empty put their values directly on the bold variable row (single-row
variables and section headings). No formulas (no recalculation needed).

Run from the project root:
    python3 R_SWD/utils/14_make_tables_xlsx.py
"""

import csv
from pathlib import Path

from openpyxl import Workbook
from openpyxl.styles import Alignment, Border, Font, Side
from openpyxl.utils import get_column_letter

PROJECT_ROOT = Path(__file__).resolve().parents[2]
REPORT_DIR = PROJECT_ROOT / "R_SWD" / "outputs" / "report"

COMBINED_XLSX_PATH = REPORT_DIR / "table1_country.xlsx"

# Each table: the source CSV, its sheet name / title in the combined workbook,
# the standalone file name / sheet name / title for the individual workbook,
# and the column widths.
SHEETS = [
    {
        "csv": "table1_country.csv",
        "sheet": "Table 1",
        "title": "Table 1. Participant characteristics by country",
        "file": "table1_characteristics.xlsx",
        "sheet_individual": "Table 1",
        "title_individual": "Table 1. Characteristics of the respondents",
        "widths": [44, 16, 16, 16],
    },
    {
        "csv": "table2_swd_subgroups.csv",
        "sheet": "Table 2",
        "title": "Table 2. Smallest worthwhile difference by subgroup",
        "file": "table2_swd_subgroups.xlsx",
        "sheet_individual": "Table 2",
        "title_individual": (
            "Table 2. Smallest worthwhile difference across drug arms and "
            "subgroups"
        ),
        "widths": [44, 8, 18, 18],
    },
    {
        "csv": "table3_swd_correlates.csv",
        "sheet": "Table 3",
        "title": (
            "Table 3. Correlates of the smallest worthwhile difference "
            "(univariable and LASSO regression, excluding never willing)"
        ),
        "file": "table3_swd_correlates.xlsx",
        "sheet_individual": "Table 3",
        "title_individual": (
            "Table 3. Univariable and LASSO correlates of the smallest "
            "worthwhile difference"
        ),
        "widths": [50, 8, 22, 10, 12],
    },
    {
        "csv": "table_s1_appendix.csv",
        "sheet": "Appendix",
        "title": (
            "Table S1. Participant characteristics by country: "
            "supplementary distributions"
        ),
        "file": "table_s1_appendix.xlsx",
        "sheet_individual": "Table S1",
        "title_individual": "Appendix Table S1. Additional characteristics",
        "widths": [44, 16, 16, 16],
    },
    {
        "csv": "table_s2_regression_sensitivity.csv",
        "sheet": "Table S2",
        "title": (
            "Table S2. Sensitivity and binary regression analyses of the "
            "smallest worthwhile difference (including never willing)"
        ),
        "file": "table_s2_regression.xlsx",
        "sheet_individual": "Table S2",
        "title_individual": (
            "Appendix Table S2. Sensitivity and binary-outcome regressions"
        ),
        "widths": [50, 8, 22, 10, 12],
    },
]

FONT_NAME = "Aptos"
BODY_FONT = Font(name=FONT_NAME, size=10)
BOLD_FONT = Font(name=FONT_NAME, size=10, bold=True)
NOTE_FONT = Font(name=FONT_NAME, size=9, italic=True)

CENTER = Alignment(horizontal="center", vertical="center")
LEFT = Alignment(horizontal="left", vertical="center")
LEFT_WRAP = Alignment(horizontal="left", vertical="top", wrap_text=True)

THIN = Side(style="thin")
MEDIUM = Side(style="medium")
HEADER_BORDER = Border(top=MEDIUM, bottom=THIN)
END_BORDER = Border(bottom=MEDIUM)

INDENT = "    "  # four spaces
NOTE_MARKER = "Note"
NOTE_COLUMN = 0
TITLE_ROW = 1
HEADER_ROW = 2
FROZEN_CELL = "A3"


def read_rows(path):
    with open(path, newline="", encoding="utf-8-sig") as handle:
        reader = csv.reader(handle)
        header = next(reader)
        rows = list(reader)
    return header, rows


def write_sheet(worksheet, title, header, rows, widths):
    n_value_columns = len(header) - 2  # variable + level + value columns
    n_displayed_columns = n_value_columns + 1  # displayed columns (A + value columns)

    # Title row
    cell = worksheet.cell(row=TITLE_ROW, column=1, value=title)
    cell.font = BOLD_FONT
    cell.alignment = LEFT

    # Header row
    header_row = HEADER_ROW
    labels = ["Characteristic"] + header[2:]
    for column, label in enumerate(labels, start=1):
        cell = worksheet.cell(row=header_row, column=column, value=label)
        cell.font = BOLD_FONT
        cell.alignment = LEFT if column == 1 else CENTER
        cell.border = HEADER_BORDER

    data_rows = [row for row in rows if row[NOTE_COLUMN] != NOTE_MARKER]
    note_rows = [row for row in rows if row[NOTE_COLUMN] == NOTE_MARKER]

    row_index = header_row
    previous_variable = None
    for record in data_rows:
        variable, level = record[0], record[1]
        values = record[2 : 2 + n_value_columns]

        if level == "":
            # Single-row variable or section heading: values on the bold row.
            row_index += 1
            cell = worksheet.cell(row=row_index, column=1, value=variable)
            cell.font = BOLD_FONT
            cell.alignment = LEFT
            for column, value in enumerate(values, start=2):
                cell = worksheet.cell(row=row_index, column=column, value=value)
                cell.font = BODY_FONT
                cell.alignment = CENTER
            previous_variable = variable
            continue

        if variable != previous_variable:
            row_index += 1
            cell = worksheet.cell(row=row_index, column=1, value=variable)
            cell.font = BOLD_FONT
            cell.alignment = LEFT
            previous_variable = variable

        row_index += 1
        cell = worksheet.cell(row=row_index, column=1, value=f"{INDENT}{level}")
        cell.font = BODY_FONT
        cell.alignment = LEFT
        for column, value in enumerate(values, start=2):
            cell = worksheet.cell(row=row_index, column=column, value=value)
            cell.font = BODY_FONT
            cell.alignment = CENTER

    # Medium border closing the table
    for column in range(1, n_displayed_columns + 1):
        worksheet.cell(row=row_index, column=column).border = END_BORDER

    # Footnotes
    for record in note_rows:
        row_index += 1
        superscript, text = record[1], record[2]
        note_text = f"{superscript} {text}".strip()
        cell = worksheet.cell(row=row_index, column=1, value=note_text)
        cell.font = NOTE_FONT
        cell.alignment = LEFT_WRAP
        worksheet.merge_cells(
            start_row=row_index, start_column=1,
            end_row=row_index, end_column=n_displayed_columns
        )

    for column, width in enumerate(widths, start=1):
        worksheet.column_dimensions[get_column_letter(column)].width = width

    # Freeze the title and header rows.
    worksheet.freeze_panes = FROZEN_CELL


def load_spec(spec):
    path = REPORT_DIR / spec["csv"]
    if not path.exists():
        raise SystemExit(f"Required input not found: {path}")
    header, rows = read_rows(path)
    expected_widths = len(header) - 1
    if len(spec["widths"]) != expected_widths:
        raise SystemExit(
            f"{spec['sheet']}: widths ({len(spec['widths'])}) do not match "
            f"displayed columns ({expected_widths})"
        )
    return header, rows


def main():
    # (1) Combined workbook with one sheet per table.
    combined = Workbook()
    combined.remove(combined.active)
    for spec in SHEETS:
        header, rows = load_spec(spec)
        worksheet = combined.create_sheet(spec["sheet"])
        write_sheet(worksheet, spec["title"], header, rows, spec["widths"])
        print(f"  [combined] Sheet '{spec['sheet']}': {worksheet.max_row} rows")
    combined.save(COMBINED_XLSX_PATH)
    print(f"Wrote {COMBINED_XLSX_PATH}")

    # (2) One standalone single-sheet workbook per table.
    for spec in SHEETS:
        header, rows = load_spec(spec)
        workbook = Workbook()
        worksheet = workbook.active
        worksheet.title = spec["sheet_individual"]
        write_sheet(worksheet, spec["title_individual"], header, rows, spec["widths"])
        output_path = REPORT_DIR / spec["file"]
        workbook.save(output_path)
        print(
            f"Wrote {output_path} (sheet '{spec['sheet_individual']}', "
            f"{worksheet.max_row} rows, {len(header) - 1} cols)"
        )


if __name__ == "__main__":
    main()
