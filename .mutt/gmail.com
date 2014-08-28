color status green default
set from = "hamhut1066@gmail.com"
set realname = "Hamish Hutchings"
#set imap_user = ""
#set imap_pass = "appsaccountpassword"
set folder = "$HOME/mail/gmail"
set spoolfile = "+inbox"
set postponed ="+bak.drafts"
#set certificate_file =~/.mutt/certificates
set sendmail="msmtp-enqueue.sh -a gmail"
