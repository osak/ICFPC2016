from slackbot.bot import respond_to


@respond_to(u'創価学会')
def test(message):
    message.reply("はわわ、危ない宗教の名前は言ってはいけマセン！")
