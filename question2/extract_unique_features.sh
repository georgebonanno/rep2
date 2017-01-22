# extracts the unique adverts by location,contact number, price, property type
# and area. The text is stored in unique_features.csv with row names on the first row.
echo "location,contact_no,price_euro,property_type,area_sqm" > unique_features.csv

# removed the data field from the csv field and extract the rest of the unique
# features.
cat extracted_features.csv | perl -pe '$_ =~ s/^[^,]+,(.*)/\1/' | sort | uniq >> unique_features.csv
