from getpass import getpass
import time, datetime
import smtplib, ssl
import subprocess

print("Starting script...")

port = 465

email = "gabrielv700@gmail.com"

reqHour = int(input("Enter the hour of update: "))
password = getpass("Please, enter your gmail password: ")

print("Starting now...")
while(True):
	timeNow = datetime.datetime.now()
	hour = timeNow.hour
	if(hour == reqHour):
		print("Sending email with csv...")
		output = subprocess.run(["Rscript", "Script.R"], text=True, capture_output=True)
		with smtplib.SMTP_SSL("smtp.gmail.com", port) as server:
			server.login(email, password)
			server.sendmail(email, email, output.stdout)
			server.close()

	print("Waiting for correct hour...")
	time.sleep(3600)
