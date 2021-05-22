import sqlite3
from seleniumwire import webdriver
from bs4 import BeautifulSoup
from datetime import datetime
import os

profile = webdriver.FirefoxProfile()
profile.set_preference('network.proxy.type', 0)
options = webdriver.FirefoxOptions()
options.headless = True         # use headless browser

driver = webdriver.Firefox(firefox_options=options, firefox_profile=profile)
driver.implicitly_wait(10)

driver.get('https://s.weibo.com/top/summary')

rank_elems = driver.find_elements_by_class_name('ranktop')

result = []
for rank_elem in rank_elems:
    topic_elem = rank_elem.find_element_by_xpath('./..')  # xpath gets parent
    td_elems = topic_elem.find_elements_by_tag_name('td')
    idx = td_elems[0].text      # topic rank / index
    if not idx.isdigit():
        continue
    a_elem = td_elems[1].find_element_by_tag_name('a')
    span_elem = td_elems[1].find_element_by_tag_name('span')
    topic = a_elem.text
    hit = int(span_elem.text)
    assert int(td_elems[0].text) == len(result)+1
    print(f'{idx:2} {topic} {hit}')
    result.append((idx, topic, hit))

today = datetime.today()

db_path = os.path.expanduser('~/eserver/weibo/hot.sqlite3')
con = sqlite3.connect(db_path)
cur = con.cursor()

try:
    cur.execute("""CREATE TABLE hot (year int, month int, day int,
                                     hour int, minute int,
                                     idx int, topic text, hit int);""")
except:
    pass

for idx, topic, hit in result:
    cur.execute('INSERT INTO hot VALUES (?, ?, ?, ?, ?, ?, ?, ?);',
                (today.year, today.month, today.day,
                 today.hour, today.minute,
                 idx, topic, hit))

driver.quit();                  # 用完一定要关了……
con.commit()
con.close()
