# WellBot

WellBot is a simple bot that can respond to a user with its own message and supports the following functional commands:
  - *__/help__*   - to see available commands
  - *__/repeat__* - to select echos amount
  
Regular backups of individual user settings are performed.
Logic of both bots is combined.

## Installing

```sh
git clone https://github.com/catdarick/WellBot
cd WellBot
stack install
mkdir -p /home/$USER/configs && cp ./templates/bot.cfg $_
```

## Usage
Copy  VK/Telegram token in config file at `/home/<user>/configs/bot.cfg`.
For VK group id is required too.

From project directory:
```sh
wellbot-exe
```



![alt text](https://github.com/catdarick/WellBot/blob/master/Demo.gif?raw=true)

