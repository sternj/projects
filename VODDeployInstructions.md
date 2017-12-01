# Instructions for compiling NGINX with Kaltura's VOD Module and PHP 7 from source
Get NGINX source
 
```bash 
wget http://nginx.org/download/nginx-1.13.1.tar.gz
tar -zxvf nginx-1.13.1.tar.gz
apt-get install git ffmpeg zlib build-essential libpcre3 libpcre3-dev libssl-dev
git clone www.github.com/kaltura/nginx-vod-module.git
cd nginx-1.13.1
./configure --with-file-aio --add-module=../nginx-vod-module
 ```
 
 
This will install nginx to /usr/local/nginx.
If you want to make starting and stopping nginx easier, run this:
```bash
nginx () {/usr/local/nginx/sbin/nginx "$@"; }
```
You will then need to install PHP.
```bash
apt-get install php7.0-fpm php7.0-dev
pecl install apcu
 ```
Before enabling PHP, you must perform some configuration.

file must be created of form `/usr/local/nginx/conf/snippets/fastcgi-php.conf` containing this
```php
# regex to split $uri to $fastcgi_script_name and $fastcgi_path
fastcgi_split_path_info ^(.+\.php)(/.+)$;
 
# Check that the PHP script exists before passing it
try_files $fastcgi_script_name =404;
 
# Bypass the fact that try_files resets $fastcgi_path_info
# see: http://trac.nginx.org/nginx/ticket/321
set $path_info $fastcgi_path_info;
fastcgi_param PATH_INFO $path_info;
 
fastcgi_index index.php;
include fastcgi.conf;
 ```
`/etc/php/7.0/fpm/pool.d/www.conf` must have line: `listen.mode = 0666s`


`/etc/php/7.0/fpm/php.ini` must have `cgi.fix_pathinfo=0`


`/etc/php/7.0/fpm/pool.d/www.conf` must have `listen=/var/run/php7.0-fpm.sock` rather than what is there


Finally, execute
```bash
sudo service php7.0-fpm restart
```
