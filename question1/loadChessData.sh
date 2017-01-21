if [ $# -lt 1 ]; then
	echo "usage: $0 <pgn files to store in db/chess.db>"
	exit 1;
fi
if [ -d db ]; then
	mkdir db;
fi
# load required gamme attribute in sqllite database
# found on db/chess.db
R --save -f chess.R --args $@

# extract the necessary statistics by executing a number
# of queries. This information is saved in workspace (.RData
# directory for the scripts that visualise the data.
R --save -f extractChessStats.R

# end of script
