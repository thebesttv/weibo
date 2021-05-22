#!/usr/bin/env python3
from multiprocessing.connection import Listener
from time import sleep
import subprocess

address = ('localhost', 23330)     # family is deduced to be 'AF_INET'
listener = Listener(address, authkey=b'weibo watchdog')

def get_connection():
    subprocess.run(['pkill', '-f', 'hotsec.py'])
    print('waiting for connection')
    conn = listener.accept()
    print(f'connection accepted from {listener.last_accepted}')
    return conn

WAIT_MAX = 10
countdown = WAIT_MAX
conn = get_connection()

while True:
    bad_connection = False
    try:
        if conn.poll():
            msg = conn.recv()
            countdown = WAIT_MAX
        else:                   # did not receive message
            msg = 'no signal'
            countdown -= 1
    except:
        bad_connection = True
        msg = 'got exception, bad connection, kill process'

    print(f'{countdown}: {msg}')

    if countdown <= 0 or bad_connection:
        print('kill process')
        subprocess.run(['pkill', '-f', 'hotsec.py'])
        countdown = WAIT_MAX

    if bad_connection:
        conn = get_connection()
    else:
        sleep(1)

listener.close()
