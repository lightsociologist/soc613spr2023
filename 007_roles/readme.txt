The file "authors.all" lists all authors included
in the Google Scholar Citation (GSC) database on June 29, 2012.
Each line corresponds to an author. For each author, we
divide data into several columns (separated by "|"). The first
column contains a "+" if the author has validated his account
with an academic email address, otherwise
it contains a "-"; the second column
reports the name of the author; the third column 
reports the affiliation of the author; 
the fourth column reports the
domain of the email address used to
validate the profile;
the fifth column reports the total number of citations
accumulated by the publications associated to the author;
the sixth column finally reports the unique ID
associated by GSC to the author.
Authors whose account was not validated
have incomplete fields, and thus a lower number
of columns.

The file "classes_authors.all" lists the keywords associated to
each author. Each line reports the data
for a single author in the following format.
There are two main columns for each line
separated by "|": the first column is the ID
of the author; the second column is the list
of keywords associated to the author. This second
column is divided in a variable number of sub-columns (separated by ";;;"),
each of them corresponding to a keyword.

The folder DATA contains a data file
for each author in the database. Files are named
using the author ID. Each files has multiple lines, each corresponding
to a publication. Each line is divided in two columns
(separated by "|") reporting the total number of citations
received by the paper and the year
of publication of the paper, respectively.


This data set can be cited as

F.Radicchi and C.Castellano
Analysis of bibliometric indicators for individual scholars in a large data set
arXiv:0689218 (2013)
accepted for publication in Scientometrics (2013)
