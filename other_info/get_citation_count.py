
def get_citation():
  import mechanicalsoup 
  import datetime

  url = 'https://scholar.google.co.uk/scholar?hl=en&as_sdt=0%2C5&q=%22Perceiving+numbers+causes+spatial+shifts+of+attention%22&btnG='

  br = mechanicalsoup.StatefulBrowser()
  br.open(url)
  cites_text = br.find_link("cite").decode_contents()
  number_of_times = cites_text[9:]

  now = datetime.datetime.now()
  current_time =  now.strftime("%d %B %Y")
  return [number_of_times,current_time]

if __name__ == '__main__':
    citations = get_citation()
    print(citations)
    
