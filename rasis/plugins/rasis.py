from slackbot.bot import respond_to
import subprocess


SEEKER_EXE = "/home/icfpc/shared/keitak/IdeaProject/zanki/seeker.py"


@respond_to(u'創価学会')
def test(message):
    message.reply("はわわ、危ない宗教の名前は言ってはいけマセン！")


@respond_to('seek (.*)')
def seek(message, name):
    message.reply('チーム {} が出題した問題の一覧デス！'.format(name))
    proc = subprocess.Popen(["/usr/bin/python", SEEKER_EXE, "-n", name], stdout=subprocess.PIPE)
    out, err = proc.communicate()
    message.reply(str(out, "utf-8"))
