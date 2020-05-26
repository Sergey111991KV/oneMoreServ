module Examples.Examples where

        
import Katip
import ClassyPrelude


runKatip :: IO ()
runKatip = withKatip $ \le ->
  runKatipContextT le () mempty logSomething
withKatip :: (LogEnv -> IO a) -> IO a
withKatip app =
  bracket createLogEnv closeScribes app
  where
    createLogEnv = do
      logEnv <- initLogEnv "HAuth" "dev"
      stdoutScribe <- mkHandleScribe ColorIfTerminal stdout (permitItem InfoS) V2
      registerScribe "stdout" stdoutScribe defaultScribeSettings logEnv
logSomething :: (KatipContext m) => m ()
logSomething = do
  $(logTM) InfoS "Log in no namespace"
  katipAddNamespace "ns1" $
    $(logTM) InfoS "Log in ns1"
  katipAddNamespace "ns2" $ do
    $(logTM) WarningS "Log in ns2"
    katipAddNamespace "ns3" $
      katipAddContext (sl "userId" $ asText "12") $ do
        $(logTM) InfoS "Log in ns2.ns3 with userId context"
        katipAddContext (sl "country" $ asText "Singapore") $
          $(logTM) InfoS "Log in ns2.ns3 with userId and country context"









--           addAuth :: PG r m
--         => D.Auth
--         -> m (Either D.RegistrationError (D.UserId, D.VerificationCode))
-- addAuth (D.Auth email pass) = do
--   let rawEmail = D.rawEmail email
--       rawPassw = D.rawPassword pass
--   -- generate vCode
--   vCode <- liftIO $ do
--     r <- stringRandomIO "[A-Za-z0-9]{16}"
--     return $ (tshow rawEmail) <> "_" <> r
--   -- issue query
--   result <- withConn $ \conn -> 
--     try $ query conn qry (rawEmail, rawPassw, vCode)
--   -- interpret result
--   case result of
--     Right [Only uId] -> return $ Right (uId, vCode)
--     Right _ -> throwString "Should not happen: PG doesn't return userId"
--     Left err@SqlError{sqlState = state, sqlErrorMsg = msg} ->
--       if state == "23505" && "auths_email_key" `isInfixOf` msg
--         then return $ Left D.RegistrationErrorEmailTaken
--         else throwString $ "Unhandled PG exception: " <> show err
--   where
--     qry = "insert into auths \
--           \(email, pass, email_verification_code, is_email_verified) \
--           \values (?, crypt(?, gen_salt('bf')), ?, 'f') returning id"
