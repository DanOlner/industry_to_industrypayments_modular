# 17_foundational_read_list.py
# Reads the Foundational Economy Collective's activity classification into a plain table.
#   Source: https://foundationaleconomy.com/activity-classification/  (the "spreadsheet" link, a list of five-digit
#   SIC 2007 codes matched to four-digit NACE Rev 2)
# The spreadsheet has no zone column. Each row is coloured, and a key at the foot gives the colours:
#   yellow = material, green = providential, orange = overlooked, no colour = other.
# So this reads each row's fill colour. Standard library only.
# Run: python3 docs/chunks/paymentflowsviz_ynh/tests/17_foundational_read_list.py <path to the downloaded .xlsx>
# Writes: docs/chunks/paymentflowsviz_ynh/tests/lookups/foundational_sic5.csv
import sys, os, re, csv, zipfile, collections
import xml.etree.ElementTree as ET

M = 'http://schemas.openxmlformats.org/spreadsheetml/2006/main'
ns = {'m': M}
zone_of_colour = {None: 'Other', 'FFFFFF00': 'Material', 'FF92D050': 'Providential', 'FFFFC000': 'Overlooked'}

z = zipfile.ZipFile(sys.argv[1])
styles = ET.fromstring(z.read('xl/styles.xml'))
fills = []
for f in styles.find('m:fills', ns):
    pf = f.find('m:patternFill', ns)
    fg = pf.find('m:fgColor', ns) if pf is not None else None
    fills.append(fg.get('rgb') if fg is not None else None)
fill_of_style = [int(x.get('fillId', 0)) for x in styles.find('m:cellXfs', ns)]
shared = [''.join(t.text or '' for t in si.iter('{%s}t' % M))
          for si in ET.fromstring(z.read('xl/sharedStrings.xml')).findall('m:si', ns)]

def text(c):
    v = c.find('m:v', ns)
    if v is None: return ''
    return shared[int(v.text)] if c.get('t') == 's' else v.text

rows = []
for r in ET.fromstring(z.read('xl/worksheets/sheet1.xml')).iter('{%s}row' % M):
    cells = {re.match(r'[A-Z]+', c.get('r')).group(0): c for c in r.findall('m:c', ns)}
    if 'A' not in cells: continue
    code = text(cells['A']).strip()
    if not re.fullmatch(r'\d{4,5}', code): continue          # headings, the key, and extra NACE lines
    colour = fills[fill_of_style[int(cells['A'].get('s', 0))]]
    if colour not in zone_of_colour: sys.exit('Unexpected colour %s on SIC %s' % (colour, code))
    rows.append([code.zfill(5), text(cells['B']).strip() if 'B' in cells else '', zone_of_colour[colour]])

out = os.path.join(os.path.dirname(os.path.abspath(__file__)), 'lookups', 'foundational_sic5.csv')
with open(out, 'w', newline='') as f:
    w = csv.writer(f); w.writerow(['sic5', 'description', 'zone']); w.writerows(rows)
print(len(rows), 'codes:', dict(collections.Counter(r[2] for r in rows)))
print('Wrote', out)
