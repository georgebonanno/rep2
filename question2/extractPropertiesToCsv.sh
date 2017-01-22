# extracts the required features from the times of malta adverts
# found in the *html* pages found under the data directory. 


# invoke the R script propertiesForSaleExtraction.R. This scripts outputs
# the features seperated by comma. The output is passed to a small perl
# program to retain only the csv data and remove the other R output.
R -f propertiesForSaleExtraction.R 2>error.log | perl -lane 'BEGIN {$i=-1;} {if ($i == 1) {$_ =~ s/^\[\d+\] "(.*)"$/\1/; if ($_ !~ /^> *$/) {print($_);}} elsif ($_ =~ /^[\+>]/) {$i=0;} elsif ($i==0 && $_ !~ /^[>\+]/) {$i=1;}} END {}' > extracted_features.csv
# store the unique adverts in a given date in extracted_with_date_unique.csv
cat extracted_features.csv | sort | uniq | grep -v ">" > extracted_with_date_unique.csv
# store the unique advert using ./extract_unique_features.sh
./extract_unique_features.sh
