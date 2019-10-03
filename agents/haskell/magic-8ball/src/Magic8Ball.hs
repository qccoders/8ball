{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Magic8Ball where

import           Data.Aeson
import           GHC.Generics
import           System.Random

data Magic8Ball =
  ItIsCertain
  | ItIsDecidedlySo
  | WithoutADoubt
  | YesDefinitley
  | YouMayRelyOnIt
  | AsISeeItYes
  | MostLikely
  | OutlookGood
  | Yes
  | SignsPointToYes
  | ReplyHazyTryAgain
  | AskAgainLater
  | BetterNotTellYouNow
  | CannotPredictNow
  | ConcentrateAndAskAgain
  | DontCountOnIt
  | MyReplyIsNo
  | MySourcesSayNo
  | OutlookNotSoGood
  | VeryDoubtful
  deriving (Eq, Ord, Enum, Generic, ToJSON, FromJSON)

ask :: IO Magic8Ball
ask = do
  result <- getStdRandom (randomR (0, fromEnum VeryDoubtful - 1))
  return $ toEnum result

instance Show Magic8Ball where
  show ItIsCertain            = "It is certain."
  show ItIsDecidedlySo        =  "It is decidedly so."
  show WithoutADoubt          = "Without a doubt."
  show YesDefinitley          = "Yes - definitely."
  show YouMayRelyOnIt         ="You may rely on it."
  show AsISeeItYes            = "As I see it, yes."
  show MostLikely             = "Most likely."
  show OutlookGood            = "Outlook good."
  show Yes                    = "Yes."
  show SignsPointToYes        = "Signs point to yes."
  show ReplyHazyTryAgain      = "Reply hazy, try again."
  show AskAgainLater          = "Ask again later."
  show BetterNotTellYouNow    = "Better not tell you know."
  show CannotPredictNow       = "Cannot predict now."
  show ConcentrateAndAskAgain = "Concentrate and ask again."
  show DontCountOnIt          = "Don't count on it."
  show MyReplyIsNo            = "My reply is no."
  show MySourcesSayNo         = "My sources say no."
  show OutlookNotSoGood       = "Outlook not so good."
  show VeryDoubtful           = "Very doubtful."
