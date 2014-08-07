color status cyan default
set from = "hamhut@hamhut1066.com"
set realname = "Hamish Hutchings"
#set imap_user = ""
#set imap_pass = "appsaccountpassword"
set folder = "$HOME/mail/hamhut1066"
set spoolfile = "+inbox"
set postponed ="+bak.drafts"
#set certificate_file =~/.mutt/certificates
set sendmail="msmtp-enqueue.sh -a hamhut1066.com"
