function fixhtdocs -d "Reset htdocs permissions"
	sudo chown -R www-data:www-data /var/www
	sudo chmod -R g+rw /var/www
end
