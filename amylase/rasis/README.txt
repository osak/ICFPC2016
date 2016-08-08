ICFPC slack bot RASIS


Installation

If you want to run this on contest EC2 instance, you don't have to do anything.

If you want to run this on the other environment like your local machine,
$ pip install -r requirements.txt


Usage

$ python rasis_say.py "Hello team."
This command lets rasis say "Hello team." in #general channel.

To change a channel,
$ python rasis_say.py -channel random "Hello random."
