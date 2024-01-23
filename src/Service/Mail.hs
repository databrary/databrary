{-# LANGUAGE OverloadedStrings #-}
module Service.Mail
  ( initMailer
  , Mailer(..)
  , MonadMail
  , sendMail
  -- below for testing
  , wrapText
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (decode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Int (Int64)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Time.Clock (getCurrentTime)
import Network.Mail.Mime
import Network.Mail.SMTP (sendMailWithLogin', sendMail')

import Has
import Model.Party
import Service.Log

-- | Make an instance of Mailer with default sendMail implementation, for use in
-- settings where mailer is not being mocked
initMailer :: Mailer
initMailer = Mailer
    { mlr = sendMailImpl
    }

-- | Default implementation of sendMail to be used in most environments besides unit tests
sendMailImpl :: MailHost -> MailPort -> MailUser -> MailPass -> Mail -> IO ()
sendMailImpl (MailHost host) (MailPort port) (MailUser "") (MailPass _) =
    sendMail' host (fromIntegral port)
sendMailImpl (MailHost host) (MailPort port) (MailUser user) (MailPass pass) =
    sendMailWithLogin' host (fromIntegral port) user pass

-- | Server hostname for smtp mail delivery
newtype MailHost = MailHost String

-- | Sever port for smtp mail delivery
newtype MailPort = MailPort Int

-- | Account username for smtp mail delivery
newtype MailUser = MailUser String

-- | Account password for smtp mail delivery
newtype MailPass = MailPass String

-- | Parts of mailer that can be mocked
data Mailer = Mailer
    { mlr :: MailHost -> MailPort -> MailUser -> MailPass -> Mail -> IO ()
    }

type MonadMail c m = (MonadLog c m, MonadHas Mailer c m)

-- |Wrap text to the given line length where possibleby changing some ' ' to '\n'.
-- >>> wrapText 10 $ TL.pack "hello there this is a test wherethereareexactlyvery long.\nLines.\n\nThing with multiple.\n"
-- "hello\nthere this\nis a test\nwherethereareexactlyvery\nlong.\nLines.\n\nThing with\nmultiple.\n"
wrapText :: Int64 -> TL.Text -> TL.Text
wrapText n = TL.unlines . concatMap wrap . TL.lines where
  wrap s
    | short s = [s]
    | (h:l) <- TL.breakOnAll " " s = let (p,r) = fb h l in p : wrap (TL.tail r)
    | otherwise = [s]
  fb p [] = p
  fb p (h@(t,_):l)
    | short t = fb h l
    | otherwise = p
  short s = TL.compareLength s n <= EQ

baseMail :: Mail
baseMail = emptyMail (Address (Just "Databrary") "help@databrary.org")

mailHeader :: TL.Text
mailHeader = TL.empty

mailFooter :: TL.Text
mailFooter = "\n\
  \Sincerely,\n\
  \The Databrary Team\n\
  \-- \n\
  \Databrary\n\
  \4 Washington Place, Room 409\n\
  \212-998-5800\n\
  \contact@databrary.org\n\
  \databrary.org\n"

sendMail :: MonadMail c m => [Either BS.ByteString Account] -> [Either BS.ByteString Account] -> T.Text -> TL.Text -> m ()
sendMail [] [] _ _ = return ()
sendMail to cc subj body = do
  mailer :: Mailer <- peek
  t <- liftIO getCurrentTime
  liftIO $ putStrLn "Retrieving mail config..."
  Just (host, port :: Int, user, pass) <- fmap decode $ liftIO $ LBS.readFile "config/email"
  focusIO $ logMsg t $ "mail " <> BS.intercalate ", " (map (either id accountEmail) to) <> ": " <> TE.encodeUtf8 subj
  liftIO $ mlr mailer (MailHost host) (MailPort port) (MailUser user) (MailPass pass) $ addPart
    [Part "text/plain; charset=utf-8" None Nothing [] $ TLE.encodeUtf8 $ mailHeader <> wrapText 78 body <> mailFooter] baseMail
    { mailTo = map addr to
    , mailCc = map addr cc
    , mailHeaders = [("Subject", subj)]
    }
  where
  addr (Left e) = Address Nothing (TE.decodeLatin1 e)
  addr (Right Account{ accountEmail = email, accountParty = p }) =
    Address (Just $ partyName $ partyRow p) (TE.decodeLatin1 email)

