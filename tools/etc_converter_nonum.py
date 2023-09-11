import sys
import re

def check_conditions(conditions, nlin, ncon, noth):
    return (
        nlin >= conditions['lines'] and 
        ncon + nlin >= conditions['lines + conics'] and
        noth >= conditions['others']
    )

def process_line(line):
    if "-Tucker-Hagos circle" in line:
        arr = line.split(" ")
        arrnew = []
        for x in arr:
            newx = x
            if "Tucker" in x:
                num = x.replace("-Tucker-Hagos","")
                newx = x.replace(num,"").replace("-Tucker-Hagos",f"Tucker-Hagos({num})")
            arrnew.append(newx)
        return " ".join(arrnew)
    else:
        return line

def process_combos(line):
    arr = line.split(" ")
    arrnew = []
    for x in arr:
        if "Y" in x or "Z" in x:
            continue
        arrnew.append(x)
    combos = " ".join(arrnew)
    if combos[-1]==",":
        combos = combos[:-1]
    return combos
    
inputfile = sys.argv[1]
outputfile = sys.argv[1]+".etc.html"

do_process_line = False
heading_tag = 'SRC:'

conditions1 = {
    'lines': 0,
    'lines + conics': 0,
    'others': 0
}

conditions2 = {
    'lines': 2,
    'lines + conics': 0,
    'others': 1
}

ifile = open(inputfile, 'r', encoding="utf-8", errors="ignore")
allines = ifile.read().splitlines() # List with stripped line-breaks
ifile.close() # Close file

nx = 62000
pts = {}
pts[nx] = []

for line in allines:
    if (heading_tag in line and "=" not in line) or line.startswith("###"):
        continue
    
    if "===" in line or "---" in line:
        nx = nx + 1
        pts[nx] = []
    if len(line.strip())>0:
        if do_process_line: #custom processing - define it in the function
            line = process_line(line)
        pts[nx].append(line)

ofile = open(outputfile, 'w', encoding="utf-8")

names = []
duplicates = []
all_curves = set()

for ptn, linearr in pts.items():

    numlines = 0
    numconics = 0
    numothers = 0

    entry = ''
    has_curves = False
    
    start_looking_for_name = True

    for line in linearr:
        if "linear combinations: " in line or line.startswith("DESCR:"):
            continue
        
        #CLEANUP RULES
        if 'X(i)-isoconjugate-of-X(j) for these {i, j}: {31,' in line and line.count(',')==2:
            continue
        if 'X(i)-Dao conjugate of X(j) for these {i, j}: {2,' in line and line.count(',')==2:
            continue
        if 'barycentric quotient X(i)/X(j) for these (i, j): {2,' in line and line.count(',')==2:
            continue
        if ', 2}' in line and 'X(i)-cross conjugate of X(j) for these {i, j}:' in line and line.count(',')==2:
            continue
        if 'X(i)-isoconjugate-of-X(j) for these {i, j}: {1,' in line and line.count(',')==2:
            continue    
        if 'X(i)-Dao conjugate of X(j) for these {i, j}: {3,' in line and line.count(',')==2:
            continue    
        if 'barycentric quotient X(i)/X(j) for these (i, j): {6,' in line and line.count(',')==2:
            continue
        if 'X(i)-vertex conjugate of X(j) for these {i, j}: {9999' in line and line.count(',')==2:
            continue

        outline = line
        line = line.strip()

        if start_looking_for_name:
            if "X(" in line or line.startswith("NAME: "):
                line = line.replace("NAME: ", "")
                if "∩" in line:
                    try:
                        pti = line.split("=")
                        lnr = sorted(pti[1].strip().split("∩"), key=lambda x: (int(x.split("X")[1].replace("(","").replace(")",""))))
                    except Exception as e:
                        print(line)
                        raise e
                    line = pti[0].strip() + " = " + "&cap;".join(lnr)
                outline = outline = '<h3 id="X'+str(ptn)+'">'+line.upper()+'</h3>\n'

                if line in names and line!="(unnamed)":
                    duplicates.append(line)
                    break
                start_looking_for_name = False
                names.append(line)
                entry = entry + outline
            continue

        if "Barycentrics " in line:
            outline = 'Barycentrics &nbsp;&nbsp; '+ line.replace('Barycentrics','').strip() +' : : \n'
            checklin = [x for x in linearr if "linear combinations: " in x]
            if len(checklin)>0:
                combos = process_combos(checklin[0]).replace('linear combinations:','=').strip()
                comboarr = combos.split(', ')
                if len(comboarr)>12:
                    comboarr = comboarr[0:12]
                    combos = ', '.join(comboarr)+' and many others'
                outline += '<b><br>\n'
                outline += combos
                outline += '\n</b>'
            #outline += '<p>\nplaceholder\n</p>'
            checklin = [x for x in linearr if x.startswith("DESCR: ")]
            if len(checklin)>10:
                outline += '<p>\n'+checklin[0].replace('DESCR: ','').strip()+'\n</p>'
            outline += "<p>\n"

        if "lies on" in line and "lines" in line:
            outline = line
            outline += '\n</p><p>\n'
            numlines = line.count('{') # number of detected lines

        if '=' in line and line.startswith("X("):
            #detect some empty lines
            if ':' in line and not '{' in line.split(':')[1]:
                continue
            outline = line + "<br>\n"
            if 'intersection' in line and 'circumconics' in line:
                numconics = line.count('{{') # number of detected circumconics
            else:
                numothers = numothers + 1

        if "= intersection" in line and line.count('{{')==0:
            continue
        
        if "pole of line" in line and line.count('{')==0:
            continue
        if "perspector of circumconic" in line and line.count('{')==0:
            continue
        if "center of circumconic" in line and line.count('{')==0:
            continue
        if 'lies on these curves' in line:
            curves = line.split(':')[1].strip()
            if ',' in curves: #only print more than 1 curve
                print (line)
                entry = entry.replace("lies on these", f"lies on {curves} and on these")
            else:
                pat = re.compile(r'^[KQ]([0-9]){3,4}$')
                is_bern =  bool(re.match(pat,curves.strip()))
                if is_bern and curves.startswith('Q'):
                    prefix = 'curve'
                elif is_bern and curves.startswith('K'):
                    prefix = 'cubic'
                else:
                    prefix = 'the'
                entry = entry.replace("lies on these", f"lies on {prefix} {curves} and on these")
            has_curves = True
            all_curves = all_curves.union(curves.split(', '))
            continue
        if "= intersection" in line and line.count('{{')==1:
            if has_curves:
                print (line)
            cconic = '{{'+line.split('{{')[-1]
            #can be either this
            if "lies on these" in entry:
                entry = entry.replace("lies on these", f"lies on circumconic {cconic} and on these")
                continue
            #or that
            if " and on these lines" in entry:
                entry = entry.replace(" and on these lines", f", circumconic {cconic}, and on these lines")
                continue

        entry = entry + outline

    #back to outer scope    
    if (
        (
            check_conditions(conditions1, numlines, numconics, numothers) or
            check_conditions(conditions2, numlines, numconics, numothers)
        )
        and len(entry)>0
    ):
        ofile.write(entry)
        ofile.write('</p><hr class="gray">\n\n')
    else:
        print(len(entry))
        print([conditions1, numlines, numconics, numothers])

ofile.close()

print(all_curves)
print(duplicates)
