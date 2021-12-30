module Email where

import Common.Lib.Prelude
import qualified Backend.Lib.Util as Util

import qualified Configuration.Dotenv as Dotenv
import qualified Network.Wreq as Wreq
import qualified Data.ByteString.Base64 as B64
import qualified Data.CaseInsensitive as CI
import qualified Control.Monad.IO.Class as IO
import qualified Snap.Core as Snap

import qualified Common.Api.User as User

configPath :: FilePath
configPath = "config/backend/email.env"

loadConfig :: Snap.Snap ()
loadConfig = do
  void $ Dotenv.loadFile $ Dotenv.defaultConfig
    { Dotenv.configPath = [configPath]
    }

defaultBaseRoute :: Text
defaultBaseRoute = "http://localhost:8000"

sendEmailVerification
  :: User.User -- ^ Receiving user
  -> Text -- ^ Secret (for URL link)
  -> Snap.Snap ()
sendEmailVerification user secret = do
  loadConfig
  baseRoute :: Text <- IO.liftIO $ Util.lookupSetting "BASE_ROUTE" defaultBaseRoute
  let link = baseRoute <> "/verify-email/" <> secret
  let subject = "Hi " <> (CI.original $ User.fullName user) <> ", please verify your Great Problems account"
  sendTemplateEmail user subject "email_verification" [("v:link", link)]

sendResetPasswordEmail
  :: User.User -- ^ Receiving user
  -> Text -- ^ Secret (for URL link)
  -> Snap.Snap ()
sendResetPasswordEmail user secret = do
  loadConfig
  baseRoute :: Text <- IO.liftIO $ Util.lookupSetting "BASE_ROUTE" defaultBaseRoute
  let link = baseRoute <> "/reset-password/" <> secret
  let subject = "Reset your Great Problems password"
  sendTemplateEmail user subject "reset_password" [("v:link", link)]

sendTemplateEmail
  :: User.User -- ^ Receiving user
  -> Text -- ^ Subject
  -> Text -- ^ Mailgun template name for body
  -> [(Text, Text)] -- ^ Mailgun template variables (key, value)
  -> Snap.Snap ()
sendTemplateEmail user subject template vars = do
  loadConfig
  mailgunApiKey :: Text <- IO.liftIO $ Util.lookupSetting "MAILGUN_API_KEY" ""
  fromDomain :: Text <- IO.liftIO $ Util.lookupSetting "FROM_DOMAIN" ""
  
  let auth = B64.encode . cs $ "api:" <> mailgunApiKey
  let url = cs $ "https://api.mailgun.net/v3/" <> fromDomain <> "/messages"
  let
    from = Wreq.partText "from" (cs $ "Great Problems Support <support@" <> fromDomain <> ">")
    to = Wreq.partText "to" (CI.original $ User.email user)
    subject' = Wreq.partText "subject" subject
    body = Wreq.partText "template" template
  let vars' = map (uncurry Wreq.partText) vars
  let opts = Wreq.defaults
        & Wreq.header "Authorization" .~ [cs $ "Basic " <> auth]
        & Wreq.header "Accept" .~ ["*/*"]
  void . IO.liftIO $ Wreq.postWith opts url ([from, to, subject', body] ++ vars')
