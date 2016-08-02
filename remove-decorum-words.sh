#!/bin/bash

#============================================================================#
#
# Description: Perl-based Bash script to replace instances of formal addresses enforced by protocol in the British House of Commons.
#
# Usage:
# ./remove-decorum-words.sh [directory]
#
# Detailed Description:
# ---Parts 1 to 3 replace all instances of "Right Honourable X", "Honourable X", "Noble Lord" and other variants, accounting
# for changes in reporting practices and abbreviations in the Hansards over the past hundred years.
# ---Part 4 replaces orphan uses of capitalized (only) instances of Honourable or Right Honourable.
# ---Part 5 replaces addresses to the Monarch.
# ---Part 6 replaces references to the numerous "Lord High"-type positions, to reduce ambiguities with the adjective "high".
# ---Part 7 replaces references to the Speech of the Throne, again to reduce ambiguities with adjectives.
# ---Part 8 replaces references to the Prime Minister, to reduce ambiguities with other uses of the adjective "prime".
# All addresses are replaced by the string FormalTitle.
#
# Authors: L. Rheault and K. Beelen
#
#============================================================================#

DIRECTORY=$1/*

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------#

perl -i -pe 's/(?:\srt\s|\srt.\s|\sright\s){1}(?:hon\s|hon.\s|honourable\s){1}/ Right Honourable /gi' $DIRECTORY
echo "Part 1 completed: Normalizing the spelling of Right Honourable."

perl -i -pe 's/(?:\shon\s|\shon.\s){1}/ Honourable /gi' $DIRECTORY
echo "Part 2 completed: Normalizing the spelling of Honourable."

perl -i -pe 's/(?:(?:right\s){0,1}(?:honourable\s|noble\s){1}(?:(?:and\s)|(?:lord\s)|(?:honourable\s)|(?:learned\s)|(?:gallant\s)|(?:noble\s)|(?:respected\s)|(?:reverend\s)){0,4}(?:the\s){0,1}(?:(?:lord(?:s){0,1})|(?:justice(?:s){0,1}\b)|(?:gentle\sm[ea]n(?:t){0,1})|(?:gentlem[ea]n(?:t){0,1})|(?:friend(?:s){0,1}\b)|(?:member(?:s){0,1}\b)|(?:lad(?:y|(?:ies)){1})){1})/FormalTitle/gi' $DIRECTORY
echo "Part 3 completed: Substituting main formal addresses."

perl -i -pe 's/Right Honourable/FormalTitle/g' $DIRECTORY
perl -i -pe 's/Honourable/FormalTitle/g' $DIRECTORY
echo "Part 4 completed: Substituting orphan instances of Honourable."

perl -i -pe 's/(?:his\s|her\s|your\s){1}(?:majesty|excellency|highness){1}(?:\sthe\sKing|\sthe\sQueen){0,1}/FormalTitle/gi' $DIRECTORY
echo "Part 5 completed: Substituting formal addresses to the Queen/King."

perl -i -pe 's/(?:lord\s){1}(?:high\s){1}(?:commissioner|admiral|chamberlain|chancellor|steward|treasurer){1}/FormalTitle/gi' $DIRECTORY
perl -i -pe 's/(?:lord\s){1}(?:high){1}/FormalTitle/gi' $DIRECTORY
echo "Part 6 completed: Substituting formal addresses to Lord High positions."

perl -i -pe "s/(?:his\s|her\s){1}(?:majesty's most gracious speech){1}/FormalTitle/gi" $DIRECTORY
perl -i -pe 's/Gracious Address/FormalTitle/gi' $DIRECTORY
perl -i -pe 's/Gracious Speech/FormalTitle/gi' $DIRECTORY
echo "Part 7 completed: Substituting references to the Gracious Address."

perl -i -pe 's/\bprime\sminister\b/FormalTitle/gi' $DIRECTORY
echo "Part 8 completed: Substituting references to Prime Minister."

perl -i -pe "s/(?:FormalTitle(?:s(?:'){0,1}|t|l|a){1}){1}/FormalTitle/g" $DIRECTORY
echo "Part 9 completed: Normalizing usage of FormalTitle placeholder."
