IFS="\n";
entries=$(cat feature_extraction.txt | perl -F, -ane 'if ($F[4] =~ 'NA') {$s=$F[2]; $s =~ s/(....)(.*)/\1 \2/; print("-e \"$F[1].*$s\" ")}')
echo "grep $entries lines.txt"


