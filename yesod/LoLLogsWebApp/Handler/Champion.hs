module Handler.Champion where

import Import

getChampionIndexR :: Handler RepHtml
getChampionIndexR = do
    items <- (runDB $ selectList [] []) :: Handler [(ChampionId, Champion)]
    defaultLayout $ do
        setTitle "Champions"
        $(widgetFile "champion/index")


getChampionViewR :: ChampionId -> Handler RepHtml
getChampionViewR modelId = do
    item <- runDB $ get404 modelId
    defaultLayout $ do
        setTitle "Champion"
        $(widgetFile "champion/view")

getChampionCreateR :: Handler RepHtml
getChampionCreateR = do
    ((_, form), enctype) <- generateFormPost (championForm Nothing)
    defaultLayout $(widgetFile "champion/create")

postChampionCreateR :: Handler RepHtml
postChampionCreateR = do
    ((result, form), enctype) <- runFormPost (championForm Nothing)
    case result of
        FormSuccess champion -> do
            modelId <- runDB $ insert champion
            redirect RedirectTemporary (ChampionViewR modelId)
        _ -> defaultLayout $(widgetFile "champion/create")


getChampionUpdateR :: ChampionId -> Handler RepHtml
getChampionUpdateR modelId = do
    item <- runDB $ get404 modelId
    ((_, form), enctype) <- generateFormPost (championForm $ Just item)
    defaultLayout $(widgetFile "champion/update")

postChampionUpdateR :: ChampionId -> Handler RepHtml
postChampionUpdateR modelId = do
    ((result, form), enctype) <- runFormPost (championForm Nothing)
    case result of
        FormSuccess champion -> do
            runDB $ replace modelId champion
            redirect RedirectTemporary (ChampionViewR modelId)
        _ -> defaultLayout $(widgetFile "champion/create")

getChampionDeleteR :: ChampionId -> Handler RepHtml
getChampionDeleteR modelId = deleteChampionDeleteR modelId
postChampionDeleteR :: ChampionId -> Handler RepHtml
postChampionDeleteR modelId = deleteChampionDeleteR modelId
deleteChampionDeleteR :: ChampionId -> Handler RepHtml
deleteChampionDeleteR modelId = do
    runDB $ delete modelId
    redirect RedirectTemporary ChampionIndexR

-- The form
championForm :: Maybe Champion -> Html -> Form LoLLogsWebApp LoLLogsWebApp (FormResult Champion, Widget)
championForm champion = renderDivs $ Champion
    <$> areq textField "Name"       (championName <$> champion)
    <*> areq urlField  "Image"      (championImageUrl <$> champion)
    <*> areq textField "Skin Name"  (championSkinName <$> champion)
