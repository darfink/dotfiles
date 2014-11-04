function sprend -d "Send files directly using Sprend" -a file email from
	if [ (count $argv) < 2 ]
		echo 'Usage: `sprend <file> <email> [<from>]`'
		return 1
	end

	if [ -z $from ]
		set from "service@sprend.com"
	end

	set uploadKey (curl 'https://www.sprend.com/generate-uploadkey.htm' ^ /dev/null)
	set result (curl -s -D - -F "UploadFile=@$file" -F "fromAddress=$from" -F "EmailAddress=$email" -F "EmailAddressMulti=$email" -F "UserMessage=" -F "UseMultiView=false" "https://www.sprend.com/u?UploadKey=$uploadKey" -o /dev/null)

	if [ $status; and (echo $result | grep 'uploadfinished') ]
		echo "File sent to: $email"
	else
		echo "File could not be sent"
	end
end
