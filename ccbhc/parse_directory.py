#!/usr/bin/env python3
"""
Parse directory_long.txt into a CSV file.
Structure:
- Place name (all caps city name, like ABBEVILLE)
- Facility name
- Address (may be multiple lines)
- Phone
- Intake phone (optional, starts with "Intake:" or "Intakes:")
- Services (all caps acronyms, may span multiple lines)

Usage:
python3 parse_directory.py directory_long.txt directory_parsed.csv
"""

import csv
import re
import sys

# Complete list of valid service codes
SERVICE_CODES = {
    'CIT', 'WI', 'MH', 'CMHC', 'MSNH', 'OMH', 'ORES', 'PH', 'PSY', 'RTCA', 'RTCC',
    'AT', 'BMT', 'CBT', 'CFT', 'DBT', 'ECT', 'GT', 'IDD', 'IPT', 'PTM', 'TELE', 'TT',
    'SMON', 'SMOP', 'SMPD', 'HI', 'OP', 'PHDT', 'RES', 'IH', 'LMG', 'OSG', 'PVT', 'SMA', 'TBG', 'VAMC',
    'CLF', 'CMHG', 'CSBG', 'ITU', 'MC', 'MD', 'MI', 'OSF', 'PI', 'SCJJ', 'SEF', 'SF', 'SI', 'SMHA', 'SWFS', 'VAF',
    'PA', 'ADM', 'ALZ', 'CJ', 'CO', 'GL', 'HV', 'MF', 'SE', 'SED', 'SMI', 'TRMA', 'VET',
    'ADLT', 'CHLD', 'SNR', 'YAD',
    'ACT', 'CDM', 'CM', 'COOT', 'DEC', 'ES', 'FPSY', 'HS', 'ICM', 'IMR', 'IPC', 'LAD', 'NRT', 'NSC', 
    'PEER', 'PRS', 'SEMP', 'SH', 'SPS', 'STU', 'TCC', 'TFC', 'VRS',
    # Additional codes found in the data
    'AH', 'SP', 'TBI', 'PTSD', 'TAY', 'TPC', 'SS', 'ACCESS', 'ACQC', 'ACTP',
    'NX', 'FX'
}

def is_place_name(line):
    """Check if line is a place name (all caps city name)"""
    line = line.strip()
    if not line:
        return False
    # Place names are all caps, no numbers, typically one or two words
    if re.match(r'^[A-Z][A-Z\s]+$', line) and not any(c.isdigit() for c in line):
        words = line.split()
        # Place names typically have 1-3 words, each 3+ chars
        # Service lines have many short words with double-spacing
        if len(words) <= 3:
            # Place names don't have double spaces
            if '  ' not in line:
                # Each word should be at least 3 chars (exclude short codes like "AH")
                # Or if 2 words, first can be shorter (e.g., "EL PASO")
                if len(words) == 1:
                    return len(words[0]) >= 4  # Single word cities are 4+ chars
                else:
                    # Multi-word: most should be 3+ chars
                    long_words = sum(1 for w in words if len(w) >= 3)
                    return long_words >= len(words) - 1
    return False

def is_valid_code(w):
    """Check if a word is a valid service/facility code"""
    return w in SERVICE_CODES or bool(re.match(r'^F\d+$', w))

def is_service_line(line):
    """Check if line contains service codes (all caps acronyms)"""
    line = line.strip()
    if not line:
        return False
    
    # Replace special separator character with space before checking
    line = line.replace('\uf073', ' ')
    line = re.sub(r'[^\x00-\x7F]', ' ', line)
    
    # Split into words and check if most/all are valid service codes
    words = line.split()
    if not words:
        return False
    
    # Count how many words are valid service codes (including F-codes like F37, F120)
    valid_codes = sum(1 for w in words if is_valid_code(w))
    
    # If majority are valid service codes, it's a service line
    # A single service code by itself is also a service line
    if len(words) == 1:
        return is_valid_code(words[0])
    else:
        return valid_codes >= len(words) * 0.5 and valid_codes >= 2

def is_phone_line(line):
    """Check if line starts with a phone number pattern"""
    line = line.strip()
    # Match (xxx) xxx-xxxx or Phone: (xxx) xxx-xxxx
    return bool(re.match(r'^\(?Phone:?\s*\)?\s*\(\d{3}\)\s*\d{3}-\d{4}', line, re.IGNORECASE)) or \
           bool(re.match(r'^\(\d{3}\)\s*\d{3}-\d{4}', line))

def is_intake_line(line):
    """Check if line starts with Intake: or Intakes:"""
    line = line.strip()
    return line.lower().startswith('intake')

def is_address_line(line):
    """Check if line looks like an address"""
    line = line.strip()
    if not line:
        return False
    # Address patterns: street number, suite/floor, city/state/zip
    if re.match(r'^\d+\s+\w', line):  # Starts with number (street address)
        return True
    if re.match(r'^[A-Z][a-z]+.*,\s*[A-Z]{2}\s+\d{5}', line):  # City, ST ZIP
        return True
    if re.match(r'^[A-Z][a-z]+.*,\s*[A-Z][a-z]+\s+\d{5}', line):  # City, State ZIP (full state name)
        return True
    if re.match(r'^Suite|^Floor|^Building|^Unit|^Room|^PO Box', line, re.IGNORECASE):
        return True
    if ',' in line and re.search(r'[A-Z]{2}\s+\d{5}', line):  # Contains state and zip
        return True
    if ',' in line and re.search(r'[A-Z][a-z]+\s+\d{5}', line):  # Contains full state name and zip
        return True
    return False

def parse_directory(input_file, output_file):
    """Parse the directory file into CSV"""
    
    with open(input_file, 'r', encoding='utf-8') as f:
        lines = f.readlines()
    
    facilities = []
    current_facility = None
    current_place = None
    name_lines = []  # Collect name lines before address
    in_services = False  # Track if we're in service section
    
    i = 0
    while i < len(lines):
        line = lines[i].strip()
        line = re.sub(r'[^\x00-\x7F]', ' ', line)
        
        # Skip empty lines
        if not line:
            # End of facility record
            if current_facility:
                # Add any pending name lines
                if name_lines and not current_facility.get('facility_name'):
                    current_facility['facility_name'] = ' - '.join(name_lines)
                    name_lines = []
                facilities.append(current_facility)
                current_facility = None
                in_services = False
            i += 1
            continue
        
        # Check for service line FIRST
        if is_service_line(line):
            in_services = True
            if current_facility:
                if 'services' not in current_facility:
                    current_facility['services'] = []
                current_facility['services'].append(line)
            i += 1
            continue
        
        # If we were in services and hit non-service, it's a new record
        if in_services and not is_service_line(line):
            if current_facility:
                facilities.append(current_facility)
                current_facility = None
            in_services = False
        
        # Check for place name
        if is_place_name(line):
            if current_facility:
                facilities.append(current_facility)
                current_facility = None
            if name_lines:
                name_lines = []
            current_place = line
            i += 1
            continue
        
        # Check for intake line
        if is_intake_line(line):
            if current_facility:
                current_facility['intake'] = line
            i += 1
            continue
        
        # Check for phone line
        if is_phone_line(line):
            # Phone indicates we've transitioned from names to details
            if name_lines and not current_facility:
                # Start a new facility with collected names
                current_facility = {
                    'place': current_place,
                    'facility_name': ' - '.join(name_lines),
                    'address': [],
                    'phone': line,
                    'intake': '',
                    'services': []
                }
                name_lines = []
            elif current_facility:
                current_facility['phone'] = line
            i += 1
            continue
        
        # Check for address line
        if is_address_line(line):
            if name_lines and not current_facility:
                # Start a new facility with collected names
                current_facility = {
                    'place': current_place,
                    'facility_name': ' - '.join(name_lines),
                    'address': [line],
                    'phone': '',
                    'intake': '',
                    'services': []
                }
                name_lines = []
            elif current_facility:
                current_facility['address'].append(line)
            i += 1
            continue
        
        # Otherwise, it's a name line
        # Collect until we hit address/phone
        name_lines.append(line)
        i += 1
    
    # Don't forget the last facility
    if current_facility:
        facilities.append(current_facility)
    
    # Write to CSV
    with open(output_file, 'w', newline='', encoding='utf-8') as f:
        writer = csv.writer(f)
        writer.writerow(['place', 'facility_name', 'address', 'phone', 'intake', 'services'])
        
        for fac in facilities:
            address = ' | '.join(fac.get('address', []))
            services = ' '.join(fac.get('services', []))
            # Clean up special character in services
            services = services.replace('\uf073', ' ')
            writer.writerow([
                fac.get('place', ''),
                fac.get('facility_name', ''),
                address,
                fac.get('phone', ''),
                fac.get('intake', ''),
                services
            ])
    
    print(f"Parsed {len(facilities)} facilities")
    print(f"Output written to {output_file}")

if __name__ == '__main__':
    if len(sys.argv) != 3:
        print("Usage: python3 parse_directory.py <input_file> <output_file>")
        sys.exit(1)
    
    parse_directory(sys.argv[1], sys.argv[2])
