# WellBot

WellBot is a simple bot that can respond to a user with its own message and supports the following functional commands:
  - *__/help__*   - to see available commands
  - *__/repeat__* - to select echos amount
  
Regular backups of individual user settings are performed.
Logic of both bots is combined.

## Architecture

Let's start with a little abstraction. Since api Vk and Telegram work in a similar way, it makes sense to combine logic together. 
The fundamental differences lie in the type of updates we receive and in the API itself. But Haskell allows us to unify the interface for interacting with API servers through classes.

So lets unify.

In the case of Telegram, we have the following set of required entities:

```haskell
newtype Chat =
  Chat
    { chatId :: Integer
    }

data Message =
  Message
    { messageMessageId :: Integer
    , messageText      :: Maybe String
    , messageChat      :: Chat
    }
    
data Update =
  Update
    { updateUpdateId :: Integer
    , updateMessage  :: Maybe Message
    }
```
Let me remind that for Vk, the set and structure of these entities will be different.

Here are the fields from the `Message` object that the bot needs to know:
- user or chat id
- message id
- sended text

Let's translate this into Haskell:
```haskell
class Message a where
  getUserOrChatId :: a -> Integer
  getMessageId    :: a -> Integer
  getMaybeText    :: a -> Maybe String
```

And we can define the instance for out data type:
```haskell
instance Message Message where
  getMaybeText = messageText 
  getUserOrChatId = chatId . messageChat 
  getMessageId = messageMessageId 
```

Now it is possible to interact with the `Message` class instance without knowing anything about its structure. Wonderful!

So let's think about the logic.

For our bot to work, we need the following methods:
- Get updates  
  Required: `offset`;
  Returns: `[Update]`; new `offset`;
- Send message  
  Required: `user or chat ID`; `text`;
- Forward message  
  Required: `user or chat ID`; `Message ID`;

They are very similar for two API's, but have some differences:

1) Vk required  initial call to get longpoll server address, offset and temp access token, using for getting updates.
2) The data obtained in the previous paragraph must be stored in the state.
3) Different types of offsets. Integer for Telegram and String for Vk (delicious).

Also for both bots, we need to store the current state of the users.
Summarizing the above, we can determine the set of necessary data that we need to store at runtime:
- offset (Integer or String)
- chat or user ID's which send `/repeat` command but haven't answered yet
- recorded repeats amount after keyboard answer
- Only for Vk - longpoll server address and temp access token

Or in Haskell:
```haskell
type UserOrChatId = Integer

type RepeatsAmount = Integer

type Chats = Map UserOrChatId RepeatsAmount

data Database offsetType additionalInfoType =
  Database
    { offset          :: offsetType
    , awaitingChatsID :: [Integer]
    , chats           :: Chats
    , additionalInfo  :: Maybe additionalInfoType
    }
  deriving (Read, Show, Eq)
```
###### Note: Field `additionalInfo` is using for some side info. For example: longpoll server address and temp access token in case of Vk.

Besides that, we need a config and the `Bot`: 

```haskell
data BotState offsetType additionalType botType =
  BotState
    { bot       :: botType
    , config    :: Config
    , database  :: Database offsetType additionalType
    } deriving (Eq, Show)

```
###### Note: This is a bit simplified version. 
`Bot` field needs to avoid explicit argument passing, but more on that later.

Since the concept of global variables does not exist in Haskell - we use the StateT monad to store this data.
And at the moment the signature of our logic function can be like:

```haskell
someFunc :: StateT (BotState offsetType additionalType botType) IO ()
```

Not very cute. For now.

So, we need to declare an interface so that universal logic can communicate with two different APIs.
Haskell's classes rescue us again:
```haskell
class Message (MessageType a) =>
      Bot a
  where
  type OffsetType a
  type MessageType a
  type AdditionalType a
  name :: a -> String
  defaultOffset :: a -> OffsetType a
  sendMessage :: a -> Config -> UserOrChatId -> String -> IO ()
  forwardMessage :: a -> Config -> UserOrChatId -> MesssageId -> IO ()
  sendKeyboardWithText :: a -> Config -> UserOrChatId -> String -> IO ()
  getUpdateMessagesAndOffset :: StateT (BotState (OffsetType a) (AdditionalType a) a) IO ([MessageType a], OffsetType a)
  initBot :: StateT (BotState (OffsetType a) (AdditionalType a) a) IO ()
```
Something horrible about the last two methods.

Let's declare some aliases:
```haskell
type BotStateT a = (StateT (BotState (OffsetType a) (AdditionalType a) a))

type BotStateIO a b = BotStateT a IO b
```

And now we can rewrite class as:
```haskell
class Message (MessageType a) =>
      Bot a
  where
  type OffsetType a
  type MessageType a
  type AdditionalType a
  name :: a -> String
  defaultOffset :: a -> OffsetType a
  sendMessage :: a -> Config -> UserOrChatId -> String -> IO ()
  forwardMessage :: a -> Config -> UserOrChatId -> MesssageId -> IO ()
  sendKeyboardWithText :: a -> Config -> UserOrChatId -> String -> IO ()
  getUpdateMessagesAndOffset :: BotStateIO a ([MessageType a], OffsetType a)
  initBot :: BotStateIO a ()
```
###### Note: The class declaration is a bit simplified. In real code, there are some minor complications for the tests to work. Instances can be seen in the code: [Telegram](/src/Telegram/Instances.hs), [Vk](/src/Vk/Instances.hs).
A little more pretty. And previously mentioned `someFunc` now be like:

```haskell
someFunc :: Bot a => BotStateIO a ()
```
So, after this actions, we can write [general logic](/src/Bot/Logic.hs) without thinking about implementing a particular method in a specific API.
## Installing

```sh
git clone https://github.com/catdarick/WellBot
cd WellBot
stack install
mkdir -p /home/$USER/configs && cp ./templates/bot.cfg $_
```

## Usage
Set VK/Telegram token in config file at `/home/<user>/configs/bot.cfg`.
For VK group ID is required too.

```sh
wellbot-exe
```



![alt text](https://github.com/catdarick/WellBot/blob/master/Demo.gif?raw=true)

