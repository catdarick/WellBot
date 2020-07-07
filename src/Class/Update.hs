module Class.Update where
class Update a where
    getMaybeText :: a -> Maybe String
    getUserOrChatId :: a -> Integer 
    getMessageId :: a -> Integer