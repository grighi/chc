#!/usr/bin/env python3
"""
Extract first two pages from 2017 MH Directory, convert from fixed-width to long format.
Automatically detects column boundaries using space distribution analysis.
"""

import sys
from collections import defaultdict

def detect_boundaries_by_space_profile(lines, n_cols=3):
    """
    Analyze vertical space distribution to find column boundaries.
    Detect sharp drops in space count to identify column boundaries.
    """

    if not lines:
        return []

    max_len = max(len(line) for line in lines)
    space_count = [0] * max_len

    for line in lines:
        line = line.ljust(max_len)
        for i, char in enumerate(line):
            if char == ' ':
                space_count[i] += 1

    boundaries = [0]

    # Look for biggest drops in specific windows
    drop_threshold = max(space_count) * 0.1

    if n_cols == 3:
        # Find biggest drop between chars 35-45
        max_drop_1 = 0
        boundary_1 = None
        for i in range(30, min(55, len(space_count))):
            if i > 0:
                drop = space_count[i-1] - space_count[i]
                if drop > max_drop_1:
                    max_drop_1 = drop
                    boundary_1 = i
                    # print(f"  Detected potential boundary at char {i} with drop {drop} (threshold {drop_threshold})")

        # Find biggest drop between chars 70-85
        max_drop_2 = 0
        boundary_2 = None
        for i in range(70, min(105, len(space_count))):
            if i > 0:
                drop = space_count[i-1] - space_count[i]
                if drop > max_drop_2:
                    max_drop_2 = drop
                    boundary_2 = i
                    # print(f"  Detected potential boundary at char {i} with drop {drop} (threshold {drop_threshold})")

        # Add boundaries if drops are significant
        if boundary_1 is not None and max_drop_1 >= drop_threshold:
            boundaries.append(boundary_1)
        if boundary_2 is not None and max_drop_2 >= drop_threshold:
            boundaries.append(boundary_2)

    if n_cols == 2:
        # Find biggest drop between chars 50-80
        max_drop_1 = 0
        boundary_1 = None
        for i in range(50, min(80, len(space_count))):
            if i > 0:
                drop = space_count[i-1] - space_count[i]
                if drop > max_drop_1:
                    max_drop_1 = drop
                    boundary_1 = i

        if boundary_1 is not None and max_drop_1 >= drop_threshold:
            boundaries.append(boundary_1)

    return sorted(boundaries), space_count


def analyze_all_pages_for_patterns(pages, n_cols=3):
    """Analyze all pages to determine column boundary patterns"""

    boundary_patterns = defaultdict(int)
    page_boundaries = {}

    for page_num, page_content in enumerate(pages):
        if not page_content.strip():
            continue

        lines = page_content.split('\n')
        content_lines = [l for l in lines if l.strip()]

        if not content_lines:
            continue

        # Detect boundaries
        boundaries, space_counts = detect_boundaries_by_space_profile(content_lines, n_cols)
        
        # Convert to tuple for use as dict key
        boundary_key = tuple(boundaries) if boundaries else None
        
        if boundary_key:
            boundary_patterns[boundary_key] += 1
            page_boundaries[page_num] = boundaries
    
    return boundary_patterns, page_boundaries

def main(input_file, output_file):
    """Extract and reformat first two pages"""

    n_cols = int(input("How many cols? (2 or 3): "))
    if n_cols not in (2, 3):
        print("Error: n_cols must be 2 or 3")
        sys.exit(1)

    with open(input_file, 'r', encoding='utf-8') as f:
        content = f.read()
    
    pages = content.split('\f')

    # Filter lines
    filtered_pages = []
    for page in pages:
        lines = page.split('\n')
        filtered_lines = []
        for line in lines:
            line_lower = line.lower()
            
            # Skip if contains unwanted text
            if 'directory of mental health facilities' in line_lower:
                continue
            if 'for code definitions' in line_lower:
                continue
            if not line.strip():  # Skip empty lines
                continue
            
            filtered_lines.append(line)
        filtered_page = '\n'.join(filtered_lines)
        filtered_pages.append(filtered_page)    

    pages = filtered_pages

    # Analyze all pages for patterns
    boundary_patterns, page_boundaries = analyze_all_pages_for_patterns(pages, n_cols)
    
    # Print summary
    most_common_pattern = max(boundary_patterns.items(), key=lambda x: x[1])
    print(f"\nTotal patterns found: {len(boundary_patterns)}")
    print(f"Total pages analyzed: {sum(boundary_patterns.values())}")
    print(f"Most common pattern: {list(most_common_pattern[0])}")
    print(f"First two boundary patterns:")
    for i in range(2):
        if i in page_boundaries:
            print(f"  Page {i}: {page_boundaries[i]}")
    print()
    
    # Get text between first and second form feed
    if len(pages) < 2:
        print("Error: File doesn't have at least 2 pages (form feed characters)")
        sys.exit(1)
    
    output_lines = []
    
    # Process first two pages individually
    for page_num in range(len(pages)):
        if page_num >= len(pages):
            break
            
        # Get page boundaries for this specific page (zero-indexed)
        if page_num not in page_boundaries:
            print(f"Warning: No boundaries detected for page {page_num}, skipping")
            continue
            
        col_boundaries = page_boundaries[page_num]
        # print(f"Processing page {page_num} with boundaries: {col_boundaries}")
        
        # Split into lines for this page
        filtered_lines = pages[page_num].split('\n')
        
        
        # Debug: if not 3 boundaries, print first 5 lines with boundary markers
        if len(col_boundaries) != n_cols:
            print(f"\nPage {page_num} has {len(col_boundaries)} boundaries (not 3): {col_boundaries}")
            print("First 5 lines with boundary markers '|':")
            for i, line in enumerate(filtered_lines[:5]):
                # Insert '|' at each boundary position
                marked_line = list(line.ljust(max(col_boundaries) + 10 if col_boundaries else len(line)))
                for boundary in sorted(col_boundaries, reverse=True):
                    if boundary < len(marked_line):
                        marked_line.insert(boundary, '|')
                print(f"  {''.join(marked_line)}")
            print()
        
        # Extract each column in sequence for this page
        for col_idx in range(len(col_boundaries)):
            col_start = col_boundaries[col_idx]
            col_end = col_boundaries[col_idx + 1] if col_idx + 1 < len(col_boundaries) else None
            
            for line in filtered_lines:
                if col_end:
                    line = line.ljust(col_end)
                    col_text = line[col_start:col_end]
                else:
                    col_text = line[col_start:]
                
                if col_text.strip():  # Only add non-empty lines
                    output_lines.append(col_text.rstrip())
            
            # Add separator between columns
            output_lines.append('')
    
    # Write output
    with open(output_file, 'w', encoding='utf-8') as f:
        f.write('\n'.join(output_lines))
    
    print(f"Extracted and reformatted {len(filtered_lines)} lines")
    print(f"Output written to {output_file}")

if __name__ == '__main__':
    if len(sys.argv) != 3:
        print("Usage: python3 extract_first_two_pages.py <input_file> <output_file>")
        sys.exit(1)
    
    main(sys.argv[1], sys.argv[2])
