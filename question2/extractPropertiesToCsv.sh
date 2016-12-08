R -f propertiesForSaleExtraction.R  | perl -pe '$_ =~ s/^\[\d+\] "(.*)"$/\1/'
