from bs4 import BeautifulSoup, SoupStrainer
import urllib.request 
import pandas as pd

URL = "https://www.theyworkforyou.com/pwdata/scrapedxml/debates/"

response = urllib.request.urlopen(URL)
soup = BeautifulSoup(response.read())

# Retrieve the url links to each debate file.
all_urls = []
for link in soup.find_all('a', href=True):
    if '.xml' in link['href']:
        all_urls.append(URL + link['href'])
        
# To save the original xml files.
for u in all_urls:
    filename = u.split('/')[-1]
    urllib.request.urlretrieve(u, filename)

# Reading the files and extracting fields of interest.
final_dataset = []
for u in all_urls:
    filename = u.split('/')[-1]
    with open(filename, 'r') as f:
        xml = f.read()
        soup = BeautifulSoup(xml , 'lxml')
        for speech in soup.find_all('speech'):
            if speech.get('speakername') is not None:     
                name = speech.get('speakername')
                speechid = speech.get('id')
                pid = speech.get('person_id')
                text = speech.find('p').get_text()
                final_dataset.append((speechid, name, pid, text))

# Saving the dataset in a csv format.
df = pd.DataFrame(final_dataset, columns=['speech_id','name', 'person_id', 'text'])
df.to_csv('uk_hansard.csv', sep='\t', index=False)
