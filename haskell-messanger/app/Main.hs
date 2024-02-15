module Main (main) where

import Types
import System.Random
import Data.Map()
import Control.Concurrent
import Data.Time.Clock
import Prelude
import Data.Typeable()
import Text.Read


{-|
    > 'randomReceiver' takes in a 'UserDetails' and a list of 'UserDetails' and
    returns a random value 'UserDetails' from the list randomReceiver :: UserDetails -> [UserDetails] -> IO UserDetails
-}
randomReceiver :: UserDetails -> [UserDetails] -> IO UserDetails
randomReceiver user usersList = do
    randomIndex <- randomRIO (0, length usersList - 1)
    let receiver = usersList!!randomIndex
    if user == receiver then
        randomReceiver user usersList
    else do
        return receiver


{-|
    > 'updateCount' takes in a list of integers and an index and returns a
    new list of integers with the element at the given index incremented by 1.
-}
updateCount :: [Int] -> Int -> [Int]
updateCount xs idx = Prelude.take idx xs ++ [xs !! idx + 1] ++ Prelude.drop (idx + 1) xs
    

{-|
    > 'messagePayload' takes in a 'user' name, a 'receiver' name, and a 'message' of type 'MessageDetails'
    and returns a new 'MessageDetails' with the current timestamp added, a random message string and
    the total count of messages incremented
-}
messagePayload :: String -> String -> MessageDetails -> IO MessageDetails
messagePayload user receiver message = do
    time_stamp <- getCurrentTime
    let stringList = ["Today is June 18th and it is Muiriel's birthday!" ,"Let's try something." ,"I have to go to sleep." ,"Muiriel is 20 now." ,"The password is 'Muiriel'." ,"I will be back soon." ,"I'm at a loss for words." ,"This is never going to end." ,"I just don't know what to say." ,"That was an evil bunny." ,"I was in the mountains." ,"Is it a recent picture?" ,"I don't know if I have the time." ,"Education in this world disappoints me." ,"You're in better shape than I am." ,"You are in my way." ,"This will cost 30." ,"I make 100 a day." ,"I may give up soon and just nap instead." ,"It's because you don't want to be alone." ,"That won't happen." ,"Sometimes he can be a strange guy." ,"I'll do my best not to disturb your studying." ,"I can only wonder if this is the same for everyone else." ,"Most people think I'm crazy." ,"No I'm not; you are!" ,"That's MY line!" ,"He's kicking me!" ,"Are you sure?" ,"Then there is a problem..." ,"Oh, there's a butterfly!" ,"Hurry up." ,"It doesn't surprise me." ,"For some reason I feel more alive at night." ,"It depends on the context." ,"Are you freaking kidding me?!" ,"That's the stupidest thing I've ever said." ,"I don't want to be lame; I want to be cool!!" ,"When I grow up, I want to be a king." ,"I'm so fat." ,"So what?" ,"I'm gonna shoot him." ,"I'm not a real fish, I'm just a mere plushy." ,"I'm just saying!" ,"That was probably what influenced their decision." ,"I've always wondered what it'd be like to have siblings." ,"This is what I would have said." ,"It would take forever for me to explain everything." ,"That's because you're a girl." ,"Sometimes I can't help showing emotions." ,"It's a word I'd like to find a substitute for." ,"It would be something I'd have to program." ,"I don't intend to be selfish." ,"Let's consider the worst that could happen." ,"How many close friends do you have?" ,"It seems interesting to me." ,"Except that here, it's not so simple." ,"I like candlelight." ,"What did you answer?" ,"No, he's not my new boyfriend." ,"It's too bad that I don't need to lose weight." ,"You never have class or what?!" ,"I will play Sudoku then instead of continuing to bother you." ,"Where is the problem?" ,"I can only wait." ,"It's not much of a surprise, is it?" ,"I love you." ,"I don't like you anymore." ,"I am curious." ,"Congratulations!" ,"I don't want to wait that long." ,"Why don't you come visit us?" ,"But the possibility seems unlikely." ,"I shouldn't have logged off." ,"It only shows you're not a robot." ,"How could I be a robot? Robots don't dream." ,"It's not something anyone can do." ,"I don't know if I still have it." ,"What do you think I've been doing?" ,"Don't underestimate my power." ,"My mom doesn't speak English very well." ,"I don't speak French well enough!" ,"I was wondering if you were going to show up today." ,"Therein lies the problem." ,"How do you find food in outer space?" ,"All you can do is trust one another." ,"Everyone wants to meet you. You're famous!" ,"Why are you sorry for something you haven't done?" ,"I utterly despise formal writing!" ,"Foreign people intrigue me." ,"Whatever I do, she says I can do better." ,"What keeps you up so late?" ,"You'd be surprised what you can learn in a week." ,"I don't have anyone who'd travel with me." ,"You're not fast enough." ,"Life is hard, but I am harder." ,"Bearing can be unbearable." ,"Stop it! You're making her feel uncomfortable!" ,"Nothing is beautiful but the truth." ,"Tomorrow, he will land on the moon." ,"I don't speak Japanese." ,"This is a pun." ,"Nobody understands me." ,"I learned to live without her." ,"It's useless to keep on thinking any more." ,"I have too many things on my mind these days." ,"I just wanted to check my email." ,"Do you really need to ask the question to know the answer?" ,"You can't expect me to always think of everything!" ,"It wasn't me, commissioner!" ,"Oh, my white pants! And they were new." ,"With so many people around he naturally became a bit nervous." ,"When I left the train station, I saw a man." ,"You're an angel!" ,"People from Madrid are weird." ,"Well, the night is quite long, isn't it?" ,"You're lucky because he didn't bite you." ,"Did you miss me?" ,"Are they all the same?" ,"Thank you very much!" ,"Where are the eggs, please?" ,"I'll take him." ,"It's a surprise." ,"That's a good idea!" ,"Round trip? Only one-way." ,"It's a pity when somebody dies." ,"They were left speechless." ,"Damn! It's not bad!" ,"Pull into shape after washing." ,"Wash before first wearing." ,"Don't open before the train stops." ,"Those who live in glass houses should not throw stones." ,"They say love is blind." ,"Oh, I'm sorry." ,"Math is like love: a simple idea, but it can get complicated." ,"The only useful answers are those that raise new qu"]
    randomIndex <- randomRIO (0,135)
    let msg = stringList!!randomIndex
    let total_count = (total_message_count message) + 1
    return MessageDetails {
        message = msg,
		timestamp = time_stamp,
		sender = user,
		receiver = receiver, 
		total_message_count = total_count
	}


{-|
    > 'messagePayload' takes in a 'user' name, a 'receiver' name, and a 'message' of type 'MessageDetails'
    and returns a new 'MessageDetails' with the current timestamp added, a random message string and
    the total count of messages incremented
-}
sendMessages :: UserDetails -> [UserDetails] -> MVar MessageDetails -> MVar [Int] -> MVar MessageDetails -> Int -> IO ()
sendMessages user usersList message globalList end messageNumber= do 
    
    -- finding a random reciever
    receiver <- randomReceiver user usersList
    
    -- takes var from shared space,one for message payload, one for the count of messages
    mVar <- takeMVar message
    list <- takeMVar globalList

    -- condition of max 100 messages
    if (total_message_count mVar) >= messageNumber then
        
        -- putting last msg back in MVar if the condition satisfies
        putMVar end mVar
    
    else do
        -- Updating the list of counts of messages received by each user
        let index = read (username receiver) :: Int
        let updatedList = (updateCount list (index-1))


        putMVar globalList updatedList

        -- generate a new message payload
        newMessage <- messagePayload (username user) (username receiver) mVar

        putStrLn $ "\n\nMessage: " ++ show (newMessage)
        putStrLn $ "Timestamp: " ++ show (timestamp newMessage)
        putStrLn $ "Sender: " ++ show (username user)
        putStrLn $ "Receiver: " ++ show (username receiver)
        putStrLn $ "User 1 Count:" ++ show (updatedList!!0)
        putStrLn $ "User 2 Count:" ++ show (updatedList!!1)
        putStrLn $ "User 3 Count:" ++ show (updatedList!!2)
        putStrLn $ "User 4 Count:" ++ show (updatedList!!3)
        putStrLn $ "User 5 Count:" ++ show (updatedList!!4)
        putStrLn $ "User 6 Count:" ++ show (updatedList!!5)
        putStrLn $ "User 7 Count:" ++ show (updatedList!!6)
        putStrLn $ "User 8 Count:" ++ show (updatedList!!7)
        putStrLn $ "User 9 Count:" ++ show (updatedList!!8)
        putStrLn $ "User 10 Count:" ++ show (updatedList!!9) 
        putStrLn $ "Total Messages Count: " ++ show (total_message_count newMessage) ++ "\n\n"

        putStrLn $ "-------------------------------------------------------------------------------------------------------------------------"

        -- put the new message in common space
        putMVar message newMessage

        threadDelay 5

        sendMessages user usersList message globalList end messageNumber


{-|
    > Generates a list of 10 users
-}
userList :: Int -> [UserDetails] -> [UserDetails]
userList 0 xs = xs
userList count xs = [UserDetails (show n) | n <- [count,count-1..1]] ++ xs


{-|
    > Interactive interface to accept users input
    Accepting the number of messages to process
    Creating a MVar list to calculate the count of messages received by each user
    Generating a list of users
    Creating a shared message variable to track messages between threads
    Declaring end variable for termination
-}
main :: IO ()
main = do
            
    putStrLn $ "\n\n---------------------------------------------"
    putStrLn "  Welcome to the Messanger app  "
    putStrLn " (1) Send Messages between users "
    putStrLn " (2) Quit"
    putStrLn $ "---------------------------------------------"
    putStrLn "Choose an option > "
    maybeInt <- fmap readMaybe getLine :: IO (Maybe Int) -- Exception handling for Int type inputs
    case maybeInt of 
        Nothing -> do 
            putStrLn "Only Integer input allowed"
            threadDelay 1000000
            main
        Just option -> do
            case option of
                1 -> do
                    putStrLn " Enter the number of messages to send (1-1000): "
                    messageNumber <- readLn :: IO Int
                    if messageNumber <= 0 || messageNumber >= 1001 then do
                        putStrLn "Please enter valid input!"
                        threadDelay 1000000
                        main
                    else do
                        --globalList :: MVar [Int]
                        globalList <- newMVar [0,0,0,0,0,0,0,0,0,0]
    
                        let usersList = userList 10 []::[UserDetails]

                        -- Creating shared Message Variable to track Messages
                        time_stamp <- getCurrentTime   
                        let messageSample = MessageDetails{sender="sender",receiver="receiver", timestamp=time_stamp, message ="message", total_message_count=0}
                        message <- newMVar messageSample

                        -- Last Message Var (So we know when to finish)
                        end <- newEmptyMVar

                        --Creating 10 individual threads to send Message to reciever
                        forkIO (sendMessages (usersList!!0) usersList message globalList end messageNumber)
                        forkIO (sendMessages (usersList!!1) usersList message globalList end messageNumber)
                        forkIO (sendMessages (usersList!!2) usersList message globalList end messageNumber)
                        forkIO (sendMessages (usersList!!3) usersList message globalList end messageNumber)
                        forkIO (sendMessages (usersList!!4) usersList message globalList end messageNumber)
                        forkIO (sendMessages (usersList!!5) usersList message globalList end messageNumber)
                        forkIO (sendMessages (usersList!!6) usersList message globalList end messageNumber)
                        forkIO (sendMessages (usersList!!7) usersList message globalList end messageNumber)
                        forkIO (sendMessages (usersList!!8) usersList message globalList end messageNumber)
                        forkIO (sendMessages (usersList!!9) usersList message globalList end messageNumber)

                        x <- takeMVar end
                        main
                2 -> putStrLn "Hope you enjoyed the messaging experience!"
                _ -> do
                    putStrLn "Please enter a valid option" -- Exception handling to ensure we enter a valid option number
                    threadDelay 1000000
                    main
