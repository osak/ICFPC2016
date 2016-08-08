from argparse import ArgumentParser

import slacker

from slackbot_settings import API_TOKEN, BOT_ICON


def main(channel, message):
    cli = slacker.Slacker(API_TOKEN)
    cli.chat.post_message(channel, message, username="rasis", icon_url=BOT_ICON)


if __name__ == '__main__':
    parser = ArgumentParser()
    parser.add_argument("-channel", default="nemsys")
    parser.add_argument("message")
    args = parser.parse_args()
    main(args.channel, args.message)

