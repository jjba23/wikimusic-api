# Here is an example on Python 3.x, much simpler than 2.x:

from smtplib import SMTP
from email.message import EmailMessage

def send_mail(to_email, subject, message, server='email-smtp.eu-west-3.amazonaws.com',
              from_email='noreply@jointhefreeworld.org'):
    # import smtplib
    msg = EmailMessage()
    msg['Subject'] = subject
    msg['From'] = from_email
    msg['To'] = ', '.join(to_email)
    msg.set_content(message)
    print(msg)
    server = SMTP(server)
    server.starttls()
    server.set_debuglevel(1)
    server.login('AKIA4DBMA2YMGZ3RGWX5', 'BB7/ZDsJIE9F+0Nxh4JhPi4c71Ax/A49KBL+OL3pCgmP')  # user & password
    server.send_message(msg)
    server.quit()
    print('successfully sent the mail.')

# call this function:

# send_mail(to_email=['12345@qq.com', '12345@126.com'],
#           subject='hello', message='Your analysis has done!')

send_mail(
    to_email = [ 'jjbigorra@gmail.com' ],
    subject = 'hello',
    message = 'email has been sent!'
)
