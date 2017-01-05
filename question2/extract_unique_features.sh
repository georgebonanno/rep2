echo "location,contact_no,price_euro,property_type,area_sqm" > unique_features.csv
cat extracted_features.csv | perl -pe '$_ =~ s/^[^,]+,(.*)/\1/' | sort | uniq >> unique_features.csv
