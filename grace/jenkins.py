import grace
import submit
import time

tmp_name = "tmp-{}".format(int(time.time()))
grace.main(tmp_name)
submit.main(tmp_name)
