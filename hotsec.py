#!/usr/bin/env python3
# 每秒爬取热搜

import os
from filelock import FileLock

dir_path = os.path.expanduser('~/eserver/weibo')

# use crontab to execute this program every minute
# use lock to ensure only one program is running at any time
lock = FileLock(os.path.join(dir_path, 'weibo-hot-sec.lock'), timeout=1)
# timeout=1: if it takes more than 1s to acquire lock, raise error

with lock:
    import requests
    import sqlite3
    import time
    from bs4 import BeautifulSoup
    from twisted.internet import task, reactor
    from multiprocessing.connection import Client

    con = sqlite3.connect(os.path.join(dir_path, 'hot-second.sqlite3'))
    cur = con.cursor()

    topic_dict = {}             # assign every topic a unique number
    try:
        cur.execute("CREATE TABLE topics (topic text primary key, idx int);")
    except:
        # if table already exists, then read into topic_dict
        for topic, idx in cur.execute("SELECT * FROM topics"):
            topic_dict[topic] = idx

    def get_topic_hash(topic):
        """get hash for topic"""
        if topic_dict.get(topic):
            return topic_dict.get(topic)
        res = topic_dict[topic] = len(topic_dict) + 1
        cur.execute("INSERT INTO topics VALUES (?, ?)", (topic, res))
        return res

    try:
        cur.execute("CREATE TABLE hotsec (timestamp int, idx int, topichash int, hit int)")
    except:
        pass

    old_result = []

    def hot_sec():
        """get current hot topic"""
        try:
            content = requests.get('https://s.weibo.com/top/summary').content
        except:
            print('execption during request, exit')
            con.commit()
            con.close()
            reactor.stop()
        soup = BeautifulSoup(content.decode('utf-8'), 'html.parser')
        rank_elems = soup.find_all(name='td', attrs={'class': 'ranktop'})
        timestamp = int(time.time())

        global old_result

        result = []

        for rank_elem in rank_elems:
            topic_elem = rank_elem.parent
            td_elems = topic_elem.find_all('td')
            idx = td_elems[0].text
            if not idx.isdigit():
                continue
            topic = td_elems[1].a.text
            topic_hash = get_topic_hash(topic)
            hit = int(td_elems[1].span.text)
            result.append((idx, topic_hash, hit))
            # print(f'{idx} {topic} {hit} {get_topic_hash(topic)}')

        if result != old_result:
            # if different from last second, then save to database
            for idx, topic_hash, hit in result:
                cur.execute("INSERT INTO hotsec VALUES (?, ?, ?, ?)",
                            (timestamp, idx, topic_hash, hit))
            msg = f'{timestamp} new'
            print(msg)  # new record
            old_result = result
        else:
            msg = f'{timestamp}   d'
            print(msg)  # duplicate record

        if time.localtime(timestamp).tm_sec == 30:
            # commit to database every minute when sec == 30
            con.commit()
            print(' commited to database')

        conn.send(msg)      # reset watchdog

    # use twisted to run function hot_sec every second
    try:
        address = ('localhost', 23330)
        conn = Client(address, authkey=b'weibo watchdog')

        tsk = task.LoopingCall(hot_sec)
        tsk.start(1)
        reactor.run()
    finally:
        print('closed')
        con.commit()
        con.close()
