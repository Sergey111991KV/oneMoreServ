
Здравствуйте! это мой очередной проект сервера - буду расчитывать, что окончательный. У всех людей есть проблемы - но я буду считать этот текстовый файл неким подобием моего общения с будущем reviever (оригинальное название может оличаться )) ) - reviewer - и в этом весь я был... смех, страдание херней, знаешь что 1 + 1 = 2 значит можно идти решать матрицы... Сейчас я постараюсь не только написать что-то стоящее, но и поменять свое мышление... Если прочитав этот обзац вы решили не читать еще одну историю человека - просто закройте и забудьте... хотя нет - я хочу чтобы вы это прочитали - для того что-бы увидеть как я кодирую и как думаю - чем низкоуровневее тем лучше! Начнем - API, WHAT IS THIS? Я вообще не понимаю как это, точнее знаю про запросы - get,post... But... Как сделать их? в Haskell? Приношу свои извинения - warp and wai - don't know... But I readed the book - "Web Development with Haskell: Master the Essential Skills to Build Fast and Scalable Web Applications" and undestended about "scotty". In my first commit I try to released in my project. Но сначала сделаю разделение в проекте - домен и адаптер, базу данных я как понял не обязательно запускать на серевере - хотя я пробовал это делать, пройдя туториал с медведем) Так как отображение http - это external entities, тогда код взаимодействия помещу в папку адаптера и сделаю функцию main в нем для запуска, а еще буду использовать classicPrelude - так как буду руководствоваться книгой. Добавлю default-extensions:

NoImplicitPrelude
OverloadedStrings - не знал этого, что так можно.

Как узнал только что - лучше readme писать в проекте - не надо запариваться с gitom по поводу -  mergen - я не работал никогда программистом, поэтому эта боль с ветками в гит мне не знакома. Пока что начну с необходимых требований - потому, что если изучать все досконально, то при том что я работаю не там где хочу, перерабатываю изрядно и еще дома семья которая требует моего внимания - я все выучу к годам так 40, а это не входит в мои планы - буду фокусироваться на главном. Хотя недавно я прошел курс по постгрессу - понял много всего для таблицы и ее построения, больше чем нужно для задания, но меньше чем можно сказать я в этом шарю)

ActionT e m a - это монада для одного маршрута.
param :: (Parsable a, ScottyError e, Monad m) => Текст -> ActionT e m a -- параметры запроса
body :: (ScottyError e, MonadIO m) => ActionT e m LByteString -- необработанное тело
jsonData :: (FromJSON a, ScottyError e, MonadIO m) => ActionT e m a -- парсит из тела
raise :: (ScottyError e, Monad m) => e -> ActionT e m a -- поднятие ошибки в монаду
rescue :: (ScottyError e, Monad m) => ActionT e m a -> (e -> ActionT e m a) -> ActionT e m a -- функция спасения, как я понял можно применить, если выскочила ошибка.
Для меня не понятен был сам запрос http - потому что я не знаю азов - поэтому когда вырасту обязательно пойду учиться этому)) поэтому мне не понятен был скорее всего бот телеграмма - я передавал там строку, без параметров и не мог понять как делать такие запросы кастово. 
setCookie :: (ScottyError e, Monad m) => SetCookie -> ActionT em () s
etCookie = addHeader "Set-Cookie". decodeUtf8. toLazyByteString. renderSetCookie
Form v m a и View v 
Перво наперво - определяем API:
блин куда то ушел не туда - начал опять все по книге делать с печеньками и аутенфикацией - буду просто использовать то что дают базовые библиотеки ... API:
для начала к каждой сущности по get..
попытавшись соединить все и сразу придется сразу добавить конфиги, scottyT  - позволит мне вставить одну монаду в другую
ага - только теперь нужна функция (m Response -> IO Response) - но тут... как подходи id??
так начну все заново - а не с API
Функционал и наследование - создав типы, напишу функции их реализации для сервера что надо. Для начала - сервис будет разделен на юзеров, авторов и админов.
CommonService - здесь будет определена общая функциональность, а итог будет в файле  mainService
Классы которые нам нужны - это взаимодействие с базой( механизм сеанса? - пока его закоментирую)
Все бред - поспал два часа на работе (нотка жалости в мой адрес))) - все пересмотрел, опять думаю другим умом - тоесть по книге, мне этого не надо - надо сделать то что нужно, а нужны операции над сущностями, и при этом новости просто расширить - это классно, ведь у всех похожий интерфейс а я его копировал - реально зачем...
 В общем придумал так сделать - будут два класса стандарт и экстенд(для новостей - ибо там больше функций) у которых будут свои методы, предворительно разделю все на папки для лучшего потом тестирования и которые будут принимать два типа данных:
 один с правами доступа, а другой это сущности