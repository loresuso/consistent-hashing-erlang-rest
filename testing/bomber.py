import requests
import sys
import time

start = 0

if len(sys.argv) < 3:
    print("Wrong usage -> python bomber.py key_base req_number")

key_base = sys.argv[1]
req_number = int(sys.argv[2], 0)
failed = 0

print("I'm going to perform {} requests".format(req_number))

for i in range(req_number):
    r = requests.put('http://192.168.1.28:8080/kvstorage-1.0/rest/kv-entries', json={
        "key": key_base + str(i+start), "value": "valore" + str(i+start)
    })
    reply = r.json()
    if r.status_code != 200:
        print("Error -> Status code: {}".format(r.status_code))
        print(r.json())
    if "data" not in reply:
        print("No data!!")
        failed += 1
    elif reply["data"] != "ok":
        print("Error -> reply['data']: {}".format(reply["data"]))
        failed += 1
    if i % 100 == 0:
        print("Done / Failed / Tot = {} / {} / {}\r".format(i+1, failed, req_number))
    #time.sleep(0.1)
