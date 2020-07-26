# WellBot

WellBot is a simple bot that can respond to a user with its own message and supports the following functional commands:
  - *__/help__*   - to select echos amount
  - *__/repeat__* - to see available commands
  
Regular backups of individual user settings are performed.
Logic of both bots is combined.

## Installing

```sh
git clone https://github.com/catdarick/WellBot
cd WellBot
stack install
```

## Usage
Set VK/Telegram token in config file at `/WellBot/app/bot.cfg`.
For VK group id is required too.

From project directory:
```sh
wellbot-exe
```



![alt text](https://github.com/catdarick/WellBot/blob/Class/Demo.gif?raw=true)

