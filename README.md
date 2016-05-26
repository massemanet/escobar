Generates html versions of your erl files

escobar reads a config file, in ~/.escobar/conf.txt. The config should specify
a destination (where to put the html files) and at least one target (where to
read .erl files from). E.g.

    {destination, "/tmp/scobar"}.
    {target, "/Users/masse/git/escobar"}.

