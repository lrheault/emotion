#!/usr/bin/python
# -*- coding: utf-8 -*-

#=====================================================================#
# 
# Description: A script to parse the British Hansard early files as stored in XML format on parliament.co.uk
# 
# Usage: 
# python early-hansard-parser.py [path-to-xml-file] [output-directory]
# 
# Authors: L. Rheault, K. Beelen 
# 
#=====================================================================#

import os, sys, re
import lxml.etree as etree

#-------------------------------------------------------------------------------------------------------------------------------------------------------------#

xmlfile = str(sys.argv[1])

xmlp = etree.parse(xmlfile)
hoc = xmlp.xpath('.//housecommons')
vol = xmlfile[25:29]
index = 0 

for h in hoc:
	for s in h.xpath('.//date'):
		date = s.attrib["format"] 
		nameout = str(sys.argv[2])+"/"+vol+"_"+str(index)+"_"+date
		with open(nameout,"w") as g:
			for c in h.xpath('.//membercontribution'):
				inner_tags = c.xpath('.//col')
				for i in inner_tags:
					etree.strip_elements(c, i.tag, with_tail=False)
				text = etree.tostring(c,method="text",encoding="utf-8",pretty_print=True)
				text = text.replace("-\n",'')
				text = text.replace("\n",' ')
				# Removes long sequences of spacing left during parsing and encodes.						
				text = re.sub('\s{2,}', ' ',text)
				# Removes long dash characters.
				text = re.sub('\xe2\x80\x94',' ',text)
				# Corrects for punctuation not followed by a space.
				text = re.sub('([.,:;!?()])', r'\1 ', text)
				# Removes empty brackets used for external links.
				text = re.sub('\[\]','',text)
				# Removes redundant spacing that may have been introduced in previous steps.
				text = re.sub('\s{2,}', ' ', text)
				if text.startswith(":") or text.startswith(",") or text.startswith(";"):
					text = text[1:]
				g.write(text+"\t"+date+"\n")
			g.close()
		index = index+1
