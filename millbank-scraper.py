#!/usr/bin/python
# -*- coding: utf-8 -*-

#======================================================================#
# 
# Description: A script to scrape and parse volumes from the UK Hansards of the 5th Series, as stored 
# on the Millbank Systems website: http://hansard.millbanksystems.com
# Requirement: Beautiful Soup v 4.
#
# Usage: 
# python millbank-scraper.py [path-to-volnumber-file] [path-to-tempdir]/ [path-to-outdir]/
#
# [path-to-volnumber-file] points to a file indicating the volume numbers to scrape and parse, i.e. a file 
# with content arranged like so:
# 2
# 4
# 184
# ...
# where the numbers on each line indicate the desired volumes.
# [path-of-tempdir] points to a directory where temporary/archive files are saved. 
# [path-of-outdir] points to a directory where the parsed debates are saved.
#
# Author: L. Rheault 
# 
#======================================================================#

import os, sys, re
import urllib
import urllib2
from bs4 import BeautifulSoup, Tag

#---------------------------------------------------------------------------------------------------------------------------------------------------------------#

# The following function is taken from: http://www.peterbe.com/plog/uniqifiers-benchmark 
def unique(seq):
	seen = set()
	return [x for x in seq if x not in seen and not seen.add(x)]

# This function retrieves the volume index from Millbank Systems, stores it, and
# parses it to retrieve unique urls containing actual debates, filtering out
# written answers to questions. 

def parseVolumeIndex (volume, path):
		# Downloads volume index:
		urlname = 'http://hansard.millbanksystems.com/volumes/5C/'+str(volume)
		text1 = urllib2.urlopen(urlname)
		
		# Parses the volume index with BeautifulSoup and returns date urls:
		soup = BeautifulSoup(text1, 'lxml')
		hlist=[]
		for a in soup.find_all('a'):
			if a['href'].startswith('/commons/'):
				hlink = 'http://hansard.millbanksystems.com'+a['href']
				shortlink = hlink[:55]
				hlist.append(shortlink)
		# Removes duplicates and preserves as an ordered set:
		hlist = unique(hlist)
		
		# Downloads each date index within volume:	
		jlist = []
		for dates in hlist:
			text2 = urllib2.urlopen(dates)
			soup2 = BeautifulSoup(text2, 'lxml')
		# Parses each date index for specific URLs:
			for a in soup2.find_all('a'):
				if a['href'].startswith('/commons/'):
					jlist.append('http://hansard.millbanksystems.com'+a['href'])
		# Removes duplicates (if any) and preserves as an ordered set:
		jlist = unique(jlist)		

		# Saves the parsed url index file in the temp folder for reference, 
		# and returns the list for future usage.
		namevolindexparsed = str(path)+str(volume)
		with open(namevolindexparsed,'w') as f:
			for item in jlist:
  				f.write('%s\n' % item)
		return jlist

# The following function downloads each section of the volume, parses them and 
# appends them as text (one speech per line) in an output file for each day, named
# after that day, and saved in the output folder.

def scrapeAndParseVolume (outpath,jlist):
	# Creates a date index for days:	
	dd=[]
	for f in jlist:
		dd.append(f[43:54].replace('/',''))
	dateset = unique(dd)

	# Parses files for each day:
	for d in dateset:
		finname = str(outpath)+str(d)
		with open(finname,'a') as h:
			for g in jlist:
				if g[43:54].replace('/','')==d:
					tempfile = urllib2.urlopen(g)
					text = tempfile.read().replace('\n','')
					soup = BeautifulSoup(text,'lxml')
					# Finds all div tags with class member_contribution:
					scriptTags = soup.find_all(class_='hentry member_contribution')
					for script in scriptTags:
						# Retrieves the Millbank Speech ID.
						if script.has_attr('id'):
							speechid = script['id']
						else:
							speechid = '__NoID__'
						# Retrieves name of speaker:
						name = ''
						try:
							citetag = script.find('cite',{'class':True})
							if 'unmatched-member' not in citetag['class']:
								name = script.find('a',{'href' : re.compile('/people/.*')})['title']
							elif 'unmatched-member' in citetag['class']:
								subscript = script.find('cite')
								name = subscript.get_text().encode('UTF-8')
							else:
								name = '__NoName__'
						except:
							name = '__MalformedData__'														
						for tag in script.find_all(['cite','span','a']):
							tag.replaceWith('')
						# Removes long sequences of spacing left during parsing and encodes.						
						s = re.sub('\s{2,}',' ',script.get_text().encode('UTF-8'))
						# Removes long dash characters.
						s = re.sub('\xe2\x80\x94',' ',s)
						# Corrects for punctuation not followed by a space.
						s = re.sub('([.,:;!?()])', r'\1 ', s)
						# Removes empty brackets used for external links.
						s = re.sub('\[\]','',s)
						# Again removes redundant spacing that may have been introduced in previous steps.
						s = re.sub('\s{2,}', ' ', s)
						# Appends the speech to the current file.	
						h.write(s+'\t'+speechid+'\t'+name+'\t'+str(d)+'\n')

if __name__ == '__main__':
	# Reads the desired list of volumes as stored in the file specified by argv[1].
	# This could be replaced by a sequence, for instance. 
	with open(str(sys.argv[1]),'r') as x:
		volumefile = x.read().splitlines()
	# Processes each volume and echoes progress to the shell.
	for v in volumefile:
		jlist = parseVolumeIndex(v, str(sys.argv[2]))
		print('Index for Volume '+str(v)+' has been retrieved.')
		scrapeAndParseVolume(str(sys.argv[3]),jlist)
		print('Volume '+str(v)+' has been processed.')
