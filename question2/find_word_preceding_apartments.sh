cat lines.txt | perl -lane 'if ($_ =~ /([^ ]+ [^ ]+ [^ ]+ apartment[^ .])/i) {print $1;}'
