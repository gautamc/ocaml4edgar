# To build without gprof:
$ corebuild -pkg re2 uniq.native 

# To build with gprof:
$ corebuild -lflag -p -pkg re2 uniq.native

To run the program:
$ ./uniq.native 
Build a list of unique values for the given field by recursively visiting index files under the root directory's daily-index and full-index sub-directories.

  uniq.native ROOT_DIR



=== flags ===

  -f field_index  Zero based index of field to extract unique values from.
  [-g group]      non-unique values
  [-p pair]       of fields to extract unique values from.
  [-build-info]   print info about this build and exit
  [-version]      print the version of this build and exit
  [-help]         print this help text and exit
                  (alias: -?)

missing required flag: -f

$ ./uniq.native -f 0 -p 1 test_data/ > output/cik_cname.txt

$ tail output/cik_cname.txt 
99197,"TRANSAMERICA INCOME SHARES, INC."
99203,"FPA NEW INCOME INC"
99250,"TRANSCONTINENTAL GAS PIPE LINE COMPANY, LLC"
99302,"TRANSCAT INC"
99359,"BREEZE-EASTERN CORP"
99547,"U. S. BOSTON CAPITAL CORPORATION"
99614,"TRI-CONTINENTAL CORP"
99771,"TRINITY CAPITAL CORP"
99780,"TRINITY INDUSTRIES INC"
9984,"BARNES GROUP INC"

$ ./uniq.native -f 0 -p 1 -g test_data/ > output/cik_cname_grouped.txt

$ $ awk -F"|" '{if(NF>2) print $0}' output/cik_cname_grouped.txt |tail
1038143|FRANCE TELECOM /|ORANGE
1035909|BROWN MICHAEL J|Brown Michael J
1026977|CITY NATIONAL ROCHDALE FUNDS|CNI CHARTER FUNDS
1021435|GRAPHON CORP/DE|hopTo Inc.
1011712|CAPITAL VENTURES INTERNATIONAL /E9/|CAPITAL VENTURES INTERNATIONAL
1009891|AIR INDUSTRIES GROUP, INC.|AIR INDUSTRIES GROUP
1531064|SPITFIRE CAPITAL LLC|Spitfire Capital LLC
1506374|ARABELLA EXPLORATION, INC.|LONE OAK ACQUISITION CORP
1487906|CACHET FINANCIAL SOLUTIONS, INC.|DE Acquisition 2, Inc.
1413547|Writ Media Group, Inc.|Writers Group Film Corp
