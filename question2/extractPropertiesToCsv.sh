R -f propertiesForSaleExtraction.R 2>error.log | perl -lane 'BEGIN {$i=-1;} {if ($i == 1) {$_ =~ s/^\[\d+\] "(.*)"$/\1/; if ($_ !~ /^> *$/) {print($_);}} elsif ($_ =~ /^[\+>]/) {$i=0;} elsif ($i==0 && $_ !~ /^[>\+]/) {$i=1;}} END {}' > extracted_features.csv
cat extracted_features.csv | sort | uniq > extracted_with_date_unique.csv
./extract_unique_features.sh
