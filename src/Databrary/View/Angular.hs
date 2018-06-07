{-# LANGUAGE CPP, OverloadedStrings #-}
module Databrary.View.Angular
  ( htmlAngular
  ) where

import Control.Monad (forM_)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BSC
import Data.Default.Class (def)
import Data.Monoid ((<>))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA

import Databrary.Has (view)
import qualified Databrary.JSON as JSON
import Databrary.Service.Types
import Databrary.Model.Identity
import Databrary.Action.Types
import Databrary.Web (WebFilePath (..))
import Databrary.Controller.Web
import Databrary.View.Html
import Databrary.View.Template

ngAttribute :: String -> H.AttributeValue -> H.Attribute
ngAttribute = H.customAttribute . H.stringTag . ("ng-" <>)

webURL :: BS.ByteString -> H.AttributeValue -- TODO: stop using this?
webURL p = actionValue webFile (Just $ StaticPath p) ([] :: Query)

versionedWebURL :: BS.ByteString -> BS.ByteString -> H.AttributeValue
versionedWebURL version p = actionValue webFile (Just $ StaticPath p) ([(version,Nothing)] :: Query)

htmlAngular :: BS.ByteString -> [WebFilePath] -> [WebFilePath] -> BSB.Builder -> RequestContext -> H.Html
htmlAngular assetsVersion cssDeps jsDeps nojs reqCtx = H.docTypeHtml H.! ngAttribute "app" "databraryModule" $ do
  H.head $ do
    htmlHeader Nothing def
    H.noscript $
      H.meta
        H.! HA.httpEquiv "Refresh"
        H.! HA.content (builderValue $ BSB.string8 "0;url=" <> nojs)
    H.meta
      H.! HA.httpEquiv "X-UA-Compatible"
      H.! HA.content "IE=edge"
    H.meta
      H.! HA.name "viewport"
      H.! HA.content "width=device-width, initial-scale=1.0, minimum-scale=1.0"
    H.title
      H.! ngAttribute "bind" (byteStringValue $ "page.display.title + ' || " <> title <> "'")
      $ H.unsafeByteString title
    forM_ [Just "114x114", Just "72x72", Nothing] $ \size ->
      H.link
        H.! HA.rel "apple-touch-icon-precomposed"
        H.! HA.href (webURL $ "icons/apple-touch-icon" <> maybe "" (BSC.cons '-') size <> ".png")
        !? (HA.sizes . byteStringValue <$> size)
    forM_ cssDeps $ \css ->
      H.link
        H.! HA.rel "stylesheet"
        H.! HA.href (versionedWebURL assetsVersion $ webFileRel css)
    H.link
      H.! HA.rel "stylesheet"
      H.! HA.href "https://allfont.net/cache/css/lucida-sans-unicode.css"
    H.link
      H.! HA.rel "stylesheet"
      H.! HA.href "https://fonts.googleapis.com/css?family=Questrial"
    H.script $ do
      H.preEscapedString "(function(w,d,s,l,i){w[l]=w[l]||[];w[l].push({'gtm.start': new Date().getTime(),event:'gtm.js'});var f=d.getElementsByTagName(s)[0], j=d.createElement(s),dl=l!='dataLayer'?'&l='+l:'';j.async=true;j.src= 'https://www.googletagmanager.com/gtm.js?id='+i+dl;f.parentNode.insertBefore(j,f); })(window,document,'script','dataLayer','GTM-NW6PSFL');"
    H.script $ do
      H.preEscapedString "window.$play={user:"
      unsafeBuilder $ JSON.fromEncoding $ JSON.recordEncoding $ identityJSON (view reqCtx)
      forM_ (serviceDown (view reqCtx)) $ \msg -> do
        H.preEscapedString ",down:"
        H.unsafeLazyByteString $ JSON.encode msg
      H.preEscapedString "};"
    forM_ jsDeps $ \js ->
      H.script
        H.! HA.src (versionedWebURL assetsVersion $ webFileRel js)
        $ return ()
  H.body
    H.! H.customAttribute "flow-prevent-drop" mempty
    $ do
    H.noscript $ do
      H.preEscapedString "Our site works best with modern browsers (Firefox, Chrome, Safari &ge;6, IE &ge;10, and others) with Javascript enabled.  You can also switch to the "
      H.a
        H.! HA.href (builderValue nojs)
        $ "simple version"
      H.preEscapedString " of this page."
    H.preEscapedString "<toolbar></toolbar>"
    H.preEscapedString $ "<main ng-view id=\"main\" class=\"main"
#ifdef SANDBOX
      <> " sandbox"
#endif
      <> "\" autoscroll ng-if=\"!page.display.error\"></main>"
    H.preEscapedString "<errors></errors>"
    htmlFooter
    H.preEscapedString "<messages></messages>"
    H.preEscapedString "<tooltip ng-repeat=\"tooltip in page.tooltips.list\"></tooltip>"
    H.div
      H.! HA.id "loading"
      H.! HA.class_ "loading"
      H.! HA.style "display:none"
      H.! ngAttribute "show" "page.display.loading" $
      H.div H.! HA.class_ "loading-animation" $ do
        H.div H.! HA.class_ "loading-spinner" $
          H.div H.! HA.class_ "loading-mask" $
            H.div H.! HA.class_ "loading-circle" $
              return ()
        H.div H.! HA.class_ "loading-text" $
          "[" >> H.span "loading" >> "]"
    H.script
      $ H.preEscapedString "document.getElementById('loading').style.display='block';"
    H.script
      $ H.preEscapedString "function initMap(){var i,e=new google.maps.LatLngBounds;i=new google.maps.Map(document.getElementById(\"map_canvas\"),{mapTypeId:\"roadmap\"});var n,t,r=[[\"Aalto University\",60.1866693,24.82768199999998],[\"Abertay University\",56.4633316,-2.9739571999999725],[\"Abraham Baldwin Agricultural College\",31.483078,-83.5301665],[\"Academia Sinica\",25.0421852,121.6145477],[\"Academic Medical Center\",52.294629,4.957973000000038],[\"Adam Mickiewicz University\",52.4083974,16.915402099999937],[\"Adelphi University\",40.719728,-73.6517116],[\"Aix Marseille University\",43.29362099999999,5.358066000000008],[\"Albert Einstein College of Medicine\",40.8521451,-73.84439380000003],[\"Albright College\",40.3615027,-75.91062899999997],[\"Alexandru Ioan Cuza University\",47.1739612,27.572106299999973],[\"Amherst College\",42.3709104,-72.5170028],[\"Appalachian State University\",36.2134842,-81.68414480000001],[\"Aristotle University of Thessaloniki\",40.6308283,22.959222400000044],[\"Arizona State University\",33.4242399,-111.92805269999997],[\"Association Italian Teachers Method Feldenkrais (AIIMF)\",43.7677075,11.2757787],[\"Australian Catholic University\",-27.3778982,153.08932170000003],[\"Babes-Bolyai University\",46.7677955,23.59127620000004],[\"Bahcesehir University (Bahçeşehir Üniversitesi)\",41.042072,29.009015099999942],[\"Banja Luka University\",44.7745476,17.211192600000004],[\"Bard College\",42.0203897,-73.91005439999998],[\"Barnard College, Columbia University\",40.8090974,-73.96396319999997],[\"Baruch College, CUNY\",40.7401991,-73.98337449999997],[\"Baylor College of Medicine\",29.71052899999999,-95.39624099999997],[\"Bilkent University\",39.8746147,32.747596199999975],[\"Binghamton University\",42.0893553,-75.96970490000001],[\"Birkbeck, University of London\",51.521975,-.13046199999996588],[\"Bordeaux INP - ENSEIRB-MATMECA\",44.80663759999999,-.6051667000000407],[\"Borough of Manhattan Community College\",40.7187801,-74.01187770000001],[\"Boston Children's Hospital\",42.3374646,-71.10532169999999],[\"Boston College\",42.3355488,-71.16849450000001],[\"Boston Medical Center\",42.3355155,-71.07285430000002],[\"Boston University\",42.3504997,-71.1053991],[\"Brigham Young University\",40.2518435,-111.64931560000002],[\"Brooklyn College, CUNY\",40.6314406,-73.95444880000002],[\"Brown University\",41.8267718,-71.40254820000001],[\"Bucknell University\",40.9546869,-76.88355139999999],[\"California Polytechnic State University, San Luis Obispo\",35.3050053,-120.66249419999997],[\"California State Polytechnic University, Pomona\",34.0575651,-117.820741],[\"California State University, Fullerton\",33.8829226,-117.88692609999998],[\"California State University, Long Beach\",33.7838235,-118.11409040000001],[\"California State University, Northridge\",34.24259070000001,-118.52771200000001],[\"California State University, Sacramento\",38.5611436,-121.42404249999998],[\"California State University, San Bernardino\",34.1821786,-117.32353239999998],[\"California State University, Stanislaus\",37.5245713,-120.85688870000001],[\"Canisius College\",42.9248338,-78.85148479999998],[\"Capella University\",44.9761068,-93.26859869999998],[\"Cardiff University\",51.48662710000001,-3.1788641000000553],[\"Carleton University\",45.3875812,-75.69602020000002],[\"Carlos Albizu University, San Juan\",18.4665523,-66.11402750000002],[\"Carnegie Mellon University\",40.4428081,-79.94301280000002],[\"Catholic University of Portugal (Universidade Catolica Portuguesa)\",41.1532817,-8.672126100000014],[\"Central European University\",47.5005395,19.04957999999999],[\"Charité - University Medicine Berlin\",52.5264618,13.376624499999934],[\"Chatham University\",40.4482193,-79.9242625],[\"Children's Hospital of Philadelphia (CHOP)\",39.9489145,-75.1939595],[\"Children’s Mercy Kansas City\",39.0837665,-94.57746320000001],[\"Christopher Newport University\",37.0646188,-76.49443839999998],[\"Cincinnati Children's Hospital Medical Center\",39.1408554,-84.50197830000002],[\"Clark University\",42.2523452,-71.82467029999998],[\"Claude Bernard University Lyon 1\",45.7788727,4.867961100000002],[\"Clemson University\",34.6760942,-82.8364148],[\"Colby College\",44.5638691,-69.66263620000001],[\"College of Staten Island, CUNY\",40.6018152,-74.14849040000001],[\"College of the Holy Cross\",42.2392391,-71.80796079999999],[\"College of William & Mary\",37.271674,-76.71337799999998],[\"Colorado College\",38.846594,-104.82439399999998],[\"Colorado State University\",40.573436,-105.0865473],[\"Columbia University\",40.8075355,-73.96257270000001],[\"Concordia University\",45.5701912,-122.63688739999998],[\"Connecticut College\",41.3786877,-72.10458690000002],[\"Copenhagen Business School\",55.681611,12.529678999999987],[\"Cornell University\",42.4534492,-76.47350269999998],[\"Curtin University\",-32.0061951,115.89441820000002],[\"Dalhousie University\",44.63658119999999,-63.5916555],[\"Dartmouth University\",43.7044406,-72.28869350000002],[\"DAV College\",30.7523403,76.7855614],[\"Davidson College\",35.5017318,-80.84677929999998],[\"Deakin University\",-37.82052789999999,144.9502334],[\"DePaul University\",41.9256494,-87.654968],[\"Duke University\",36.0014258,-78.9382286],[\"Duquesne University\",40.4372949,-79.99017279999998],[\"Earlham College\",39.8208853,-84.913052],[\"East Carolina University\",35.6055108,-77.36456529999998],[\"Eastern Michigan University\",42.2506803,-83.62408900000003],[\"East Tennessee State University\",36.3025374,-82.37019329999998],[\"Edogawa University\",35.8765371,139.93873929999995],[\"Emory University\",33.7925195,-84.32399889999999],[\"Farmingdale State College\",40.752182,-73.42244599999998],[\"Federal University of Alagoas\",-9.554630300000001,-35.776021300000025],[\"Federal University of Pará\",-1.4743965,-48.453221799999994],[\"Federal University of Rio Grande do Sul (Universidade Federal do Rio Grande do Sul)\",-30.0338411,-51.21857019999999],[\"Federal University of Rio Grande North\",-5.8393707,-35.200772700000016],[\"Florida International University\",25.756576,-80.37394899999998],[\"Florida State University\",30.4418778,-84.2984889],[\"Foundation CTIC\",43.521331,-5.609998],[\"Franklin and Marshall College\",40.0475258,-76.31790160000003],[\"Free University of Berlin (Freie Universität)\",52.4525264,13.289678699999968],[\"Gallaudet University\",38.908422,-76.9923875],[\"Gangneung-Wonju National University\",37.7687678,128.87014090000002],[\"General Hospital of Mexico\",39.1725348,-91.8773415],[\"George Mason University\",38.8315541,-77.31208850000002],[\"Georgetown University\",38.9076089,-77.07225849999998],[\"George Washington University\",38.8997145,-77.04859920000001],[\"Georgia Institute of Technology\",33.7756178,-84.39628499999998],[\"Georgia State University\",33.753068,-84.3852819],[\"Gettysburg College\",39.8362073,-77.2375078],[\"Goethe-University\",50.1270675,8.667763499999978],[\"Goldsmiths University of London\",51.47427099999999,-.03540799999996125],[\"Graduate Center, CUNY\",40.748449,-73.98349159999998],[\"Grand Valley State University\",42.9639355,-85.88894649999997],[\"Grinnell College\",41.74905709999999,-92.72013019999997],[\"Gustavus Adolphus College\",44.3221619,-93.96877710000001],[\"Hangzhou Normal University\",30.289532,120.00988600000005],[\"Harvard University\",42.3770029,-71.11666009999999],[\"Hofstra University\",40.716792,-73.59940410000002],[\"Hong Kong University of Science and Technology\",22.3363998,114.2654655],[\"Humboldt State University\",40.8752748,-124.07782099999997],[\"Humboldt-University of Berlin\",52.517883,13.393655099999933],[\"Hunter College, CUNY\",40.7685406,-73.96462509999998],[\"IE University\",40.9528125,-4.118795699999964],[\"Illinois Institute of Technology\",41.8348731,-87.62700589999997],[\"Indiana University\",39.1745704,-86.51294580000001],[\"Indiana University Kokomo\",40.4596897,-86.13144360000001],[\"Indian Institute of Science\",13.0218597,77.5671423],[\"Indraprastha Institute of Information Technology Delhi\",28.5456282,77.27315049999993],[\"Infantium\",41.38506389999999,2.1734035],[\"INSERM\",45.740361,4.893118700000059],[\"Institute of Business Administration, Karachi\",24.9406382,67.11448859999996],[\"Institute of Psychology, Chinese Academy of Sciences\",40.00529909999999,116.37602600000002],[\"Interdisciplinary Center Herzliya\",32.176113,34.83623399999999],[\"IRCCS Stella Maris Foundation\",43.6077483,10.292125899999974],[\"ISCTE - University Institute of Lisbon (IUL)\",38.7478406,-9.153442799999993],[\"Isfahan University of Medical Sciences\",32.6128887,51.66168719999996],[\"Istanbul Technical University\",41.1055941,29.025340099999994],[\"Italian Hospital of Buenos Aires\",-34.6072209,-58.42627700000003],[\"ITESO University (Instituto Tecnologico y de Estudios Superiores de Occidente)\",20.6072093,-103.4155404],[\"Jagiellonian University\",50.0609623,19.934107400000016],[\"Japan Advanced Institute of Science and Technology\",36.444154,136.5930972],[\"Johns Hopkins University\",39.3299013,-76.6205177],[\"Kansas State University\",39.1974437,-96.58472489999997],[\"Keele University\",53.00343729999999,-2.272053000000028],[\"King Khalid University\",18.2488367,42.55953929999998],[\"Kingston University London\",51.4032431,-.30348819999994703],[\"Kobe University\",34.7256185,135.23539529999994],[\"Koc University\",41.19779640000001,29.067383500000005],[\"Kumamoto University\",32.8140382,130.7278725],[\"Kutztown University of Pennsylvania\",40.5100878,-75.78342250000003],[\"Kyoto University\",35.0262444,135.7808218],[\"Kyushu University\",33.6266584,130.4250445],[\"Laboratoire de Sciences Cognitives et Psycholinguistique\",48.84353040000001,2.3448876],[\"Lafayette College\",40.6986202,-75.20872980000001],[\"Lancaster University\",54.0103942,-2.7877293999999893],[\"Lehigh University\",40.6069087,-75.3782832],[\"Lehman College, CUNY\",40.87331830000001,-73.8941395],[\"Leiden University\",52.1571485,4.485208999999941],[\"Leipzig University (Universität Leipzig)\",51.3385738,12.378461499999958],[\"Liverpool Hope University\",53.3908007,-2.892298799999935],[\"Lock Haven University\",41.1423205,-77.46214750000001],[\"London School of Economics and Political Science\",51.5144077,-.11737659999994321],[\"London South Bank University\",51.498224,-.10208620000003066],[\"Louisiana State University\",30.4132579,-91.18000230000001],[\"Loyola University Chicago\",41.998997,-87.65819210000001],[\"Ludwig Maximilian University of Munich\",48.150806,11.580429999999978],[\"Lynchburg College\",37.4006,-79.183153],[\"Macquarie University\",-33.77382370000001,151.11264979999999],[\"Marshall University\",38.4235332,-82.4263138],[\"Massachusetts Institute of Technology\",42.360091,-71.09415999999999],[\"Max Planck Institute for Evolutionary Anthropology\",51.3211759,12.395021400000019],[\"Max Planck Institute for Human Development\",52.468554,13.303832000000057],[\"Max Planck Institute for Psycholinguistics\",51.81799340000001,5.85709339999994],[\"McGill University\",45.50478469999999,-73.57715109999998],[\"McMaster University\",43.260879,-79.91922540000002],[\"Memorial University\",47.5737975,-52.73290529999997],[\"Mercy College\",41.0221144,-73.87453909999999],[\"Metropolitan State University\",44.9569973,-93.07424259999999],[\"Miami University\",39.5105334,-84.73087679999998],[\"Michigan State University\",42.701848,-84.48217190000003],[\"Michigan Technological University\",47.1149065,-88.54530729999999],[\"Middlesex University\",51.589755,-.2282989999999927],[\"Millersville University\",39.9976889,-76.3544283],[\"Montana State University\",45.6667557,-111.04980999999998],[\"Montclair State University\",40.8644792,-74.19855969999998],[\"Mount Saint Mary's University\",39.6807058,-77.3490013],[\"Narsee Monjee Institute of Management Studies\",19.110093,72.83748029999992],[\"National Autonomous University of Mexico\",19.3188895,-99.18436759999997],[\"National Institute of Astrophysics, Optics and Electronics\",19.0323107,-98.31537019999996],[\"National Institute of Child Health & Human Development NICHD\",39.0292164,-77.13551009999998],[\"National Institute of Mental Health (NUDZ/ NIMH)\",49.81749199999999,15.472962],[\"National Institute of Technology Durgapur\",23.5476728,87.29313890000003],[\"National Institute of Technology Rourkela\",22.2534697,84.90113210000004],[\"National Science Foundation (NSF)\",38.8016276,-77.0704465],[\"National Scientific and Technical Research Council (CONICET)\",-34.5826428,-58.4290725],[\"National Taipei University of Technology\",25.0422329,121.53549739999994],[\"National University of Córdoba\",-31.4354855,-64.18557020000003],[\"National University of La Plata\",-34.9128319,-57.951162899999986],[\"National University of Singapore\",1.2966426,103.77639390000002],[\"Nationwide Children's Hospital\",39.9531241,-82.97957429999997],[\"Nencki Institute of Experimental Biology, Polish Academy of Sciences\",52.2296756,21.0122287],[\"Newcastle University\",54.9791871,-1.6146608000000242],[\"New College of Florida\",27.384828,-82.55870299999998],[\"New School for Social Research\",40.735547,-73.99429700000002],[\"New York State Institute for Basic Research in Developmental Disabilities\",40.5976369,-74.14218540000002],[\"New York University, School of Medicine\",40.7420088,-73.97422819999997],[\"New York University (NYU)\",40.72951339999999,-73.99646089999999],[\"Niagara University\",43.1365152,-79.0353197],[\"Niigata University\",37.86701,138.942539],[\"Nile University of Nigeria\",9.0188233,7.397756800000025],[\"North Dakota State University\",46.8977528,-96.80243669999999],[\"Northeastern University\",42.3398067,-71.08917170000001],[\"Northern Illinois University\",41.934233,-88.774069],[\"Northwestern University\",42.0564594,-87.67526699999996],[\"Norwegian University of Science and Technology\",63.41949899999999,10.402077100000042],[\"Oberlin College\",41.29348299999999,-82.2236001],[\"Ohio State University\",40.0141905,-83.0309143],[\"Ohio State University, Lima\",40.7372754,-84.02834630000001],[\"Okinawa Institute of Science and Technology\",26.465329,127.82967200000007],[\"Oklahoma State University\",36.1270236,-97.07372220000002],[\"Old Dominion University\",36.8856104,-76.3067777],[\"Omsk State Pedagogical University\",54.99007959999999,73.35769959999993],[\"Oregon Health and Science University\",45.4962147,-122.62625639999999],[\"Oregon State University\",44.5637806,-123.27944430000002],[\"Osaka University\",34.8220139,135.52446759999998],[\"Oxford Brookes University\",51.755011,-1.224224999999933],[\"Ozyegin University\",41.0313315,29.25871559999996],[\"Pace University\",40.7111197,-74.0048567],[\"Pakistan Institute of Learning and Living\",24.8178243,67.04061300000001],[\"Paris Descartes University\",48.851251,2.3407609999999295],[\"Peking University First Hopsital\",39.931765,116.38041599999997],[\"Penn State University, Abington\",40.115494,-75.11028799999997],[\"Penn State University, Brandywine\",39.9278884,-75.44814159999999],[\"Penn State University, University Park\",40.7982133,-77.8599084],[\"Penn State University, Worthington-Scranton\",41.4401197,-75.62083689999997],[\"Pomona College\",34.097743,-117.71180300000003],[\"Pontifical Catholic University of Rio Grande do Sul (PUCRS)\",-30.0593446,-51.1734912],[\"Pontificia Universidad Católica de Chile\",-33.4411279,-70.64079329999998],[\"Princeton University\",40.3439888,-74.65144809999998],[\"Providence College\",41.8440925,-71.43816529999998],[\"Punjab University\",31.47898409999999,74.2661627],[\"Purdue University\",40.4237054,-86.92119459999998],[\"Queen's University\",44.2252795,-76.49514119999998],[\"Queen's University, Belfast\",54.5844087,-5.93404929999997],[\"Queens College, CUNY\",40.7379735,-73.81723929999998],[\"Radboud University\",51.8193148,5.856887700000016],[\"Ramapo College\",41.0815079,-74.17462339999997],[\"Randolph-Macon College\",37.7603794,-77.47835179999998],[\"Rhodes College\",35.1550839,-89.98928719999998],[\"Rice University\",29.7173941,-95.4018312],[\"Rikkyo University\",35.730506,139.704029],[\"Rochester Institute of Technology\",43.0861017,-77.67051429999998],[\"Roosevelt University\",41.8763057,-87.62511330000001],[\"Rosalind Franklin University of Medicine and Science\",42.300356,-87.8586267],[\"Rutgers University, Newark\",40.741187,-74.17530950000003],[\"Rutgers University, New Brunswick\",40.5008186,-74.44739909999998],[\"Ryerson University\",43.6576585,-79.3788017],[\"Saarland University\",49.2550284,7.040975000000003],[\"Sabanci University\",40.8918026,29.37646510000002],[\"Sacred Heart University\",41.2203902,-73.24330029999999],[\"Saint Joseph's University\",39.9946832,-75.24164050000002],[\"Sakushin Gakuin University\",36.542789,139.97745699999996],[\"Sam Houston State University\",30.7131978,-95.55035659999999],[\"San Diego State University\",32.77572170000001,-117.07188930000001],[\"San Francisco State University\",37.721897,-122.47820939999997],[\"San Jose State University\",37.3351874,-121.88107150000002],[\"Santa Clara University\",37.3496418,-121.9389875],[\"Sapienza University of Rome\",41.9037626,12.514438400000017],[\"Seoul National University\",37.459882,126.95190530000002],[\"Seton Hall University\",40.743011,-74.24710010000001],[\"Simon Fraser University\",49.2780937,-122.91988329999998],[\"Singapore University of Technology and Design\",1.3402566,103.96294910000006],[\"Skidmore College\",43.0972759,-73.78418110000001],[\"Slippery Rock University\",41.06302489999999,-80.04116909999999],[\"Smith-Kettlewell Eye Research Institute\",37.7912559,-122.43418309999998],[\"Southern Illinois University\",37.7090577,-89.22491339999999],[\"Southern New Hampshire University\",43.0407729,-71.45318359999999],[\"Southwestern University\",30.6349251,-97.6651268],[\"St. Francis Xavier University\",45.6179109,-61.99544159999999],[\"St. John's University\",40.7215967,-73.79468989999998],[\"St. Lawrence University\",44.5892119,-75.1608814],[\"St. Mary's College of Maryland\",38.1909255,-76.42903130000002],[\"Stanford University\",37.4274745,-122.16971899999999],[\"State University of New York at Fredonia\",42.4538665,-79.34036329999998],[\"Stockholm University\",59.36276469999999,18.059267699999964],[\"Sun Yat-Sen University\",23.0965384,113.29888299999993],[\"Swarthmore\",39.9068405,-75.35564829999998],[\"Swinburne University of Technology\",-37.8221504,145.0389546],[\"SWPS University of Social Sciences and Humanities\",52.2484404,21.065015500000072],[\"Syracuse University\",43.0391534,-76.1351158],[\"Teachers College, Columbia University\",40.8101861,-73.96038049999999],[\"Technion – Israel Institute of Technology\",32.7767783,35.02312710000001],[\"Teesside University\",54.5706735,-1.235274200000049],[\"Tel Aviv University\",32.1133141,34.80438770000001],[\"Temple University\",39.9811935,-75.15535119999998],[\"Texas A&M University\",30.618531,-96.336499],[\"Texas Christian University\",32.70953,-97.362795],[\"Texas Woman's University\",33.2262879,-97.12710340000001],[\"The Catholic University of America\",38.9368811,-76.998692],[\"The University of British Columbia\",49.26060520000001,-123.24599380000001],[\"The University of Newcastle\",-32.8927718,151.70417750000001],[\"The University of Queensland\",-27.4954306,153.01203009999995],[\"The University of Scranton\",41.40554119999999,-75.65697840000001],[\"The University of Sydney\",-33.888584,151.18734730000006],[\"The University of Texas at Arlington\",32.7301078,-97.11625240000001],[\"The University of Texas at Austin\",30.2849185,-97.7340567],[\"The University of Texas at Dallas\",32.9857619,-96.75009929999999],[\"The University of Western Australia\",-31.981179,115.81990960000007],[\"Thompson Rivers University\",50.672496,-120.37061599999998],[\"Tokyo Metropolitan University\",35.617166,139.37703299999998],[\"Toulouse University\",43.5943757,1.4506493999999748],[\"Trent University\",44.3571304,-78.29036300000001],[\"Trinity College\",41.7479332,-72.6903345],[\"Trinity College Dublin\",53.3437935,-6.254571599999963],[\"Tufts University\",42.4074843,-71.11902320000002],[\"Tulane University\",29.9403477,-90.12072790000002],[\"Ulm University\",48.4222305,9.95558200000005],[\"Ulster University\",55.00643840000001,-7.324364400000036],[\"Universidade Federal do Paraná\",-25.4269081,-49.261765800000035],[\"Université du Québec à Trois-Rivières\",46.3471542,-72.5768812],[\"Université du Québec en Outaouais\",45.4224541,-75.738405],[\"Université Libre de Bruxelles\",50.8132068,4.382222200000001],[\"Université Paul-Valéry Montpellier 3\",43.6324414,3.8702429999999595],[\"Universiti Putra Malaysia\",2.991686,101.71629000000007],[\"University at Buffalo\",43.0008093,-78.7889697],[\"University College London\",51.52455920000001,-.1340400999999929],[\"University Duesseldorf (Heinrich-Heine-Universität Düsseldorf)\",51.1863034,6.79606960000001],[\"University Ibn Zohr (Université Ibn Zohr)\",30.4109818,-9.54347400000006],[\"University of Akron\",41.076655,-81.51133859999999],[\"University of Alberta\",53.5232189,-113.52631859999997],[\"University of Algarve\",37.0439713,-7.972207900000058],[\"University of Angers\",47.47709099999999,-.5499306000000388],[\"University of Arizona\",32.2318851,-110.95010939999997],[\"University of Arkansas\",36.0678324,-94.17365510000002],[\"University of Arkansas at Little Rock\",34.7240568,-92.33888719999999],[\"University of Auckland\",-36.8523378,174.76910729999997],[\"University of Bath\",51.3781162,-2.327263499999958],[\"University of Brazil (Universidade de Brasilia)\",-15.7631573,-47.87063109999997],[\"University of Cagliari\",39.21736569999999,9.114921800000047],[\"University of Calgary\",51.0781599,-114.1358007],[\"University of California, Berkeley\",37.8718992,-122.25853990000002],[\"University of California, Davis\",38.5382322,-121.76171249999999],[\"University of California, Irvine\",33.6404952,-117.84429620000003],[\"University of California, Los Angeles\",34.068921,-118.44518110000001],[\"University of California, Merced\",37.3648748,-120.42540020000001],[\"University of California, Riverside\",33.9737055,-117.32806440000002],[\"University of California, San Diego\",32.8800604,-117.2340135],[\"University of California, San Francisco\",37.7631333,-122.4575489],[\"University of California, Santa Barbara\",34.4139629,-119.84894700000001],[\"University of California, Santa Cruz\",36.9914738,-122.05829719999997],[\"University of Cambridge\",52.2042666,.1149084999999559],[\"University of Campinas\",-22.8184393,-47.06472059999999],[\"University of Canterbury\",-43.5235375,172.58392330000004],[\"University of Central Florida\",28.6024274,-81.20005989999999],[\"University of Central Lancashire\",53.7645034,-2.7083505000000514],[\"University of Chicago\",41.7886079,-87.59871329999999],[\"University of Cleveland\",41.50249729999999,-81.67471849999998],[\"University of Cologne\",50.9281625,6.928819200000021],[\"University of Colorado, Boulder\",40.00758099999999,-105.26594169999998],[\"University of Colorado, Colorado Springs\",38.8971008,-104.8061611],[\"University of Connecticut\",41.8077414,-72.25398050000001],[\"University of Cyprus\",35.1600014,33.377017499999965],[\"University of Delaware\",39.6779504,-75.75061140000003],[\"University of Denver\",39.6766174,-104.96189649999997],[\"University of East Anglia\",52.6219215,1.2391761000000088],[\"University of East London\",51.5076024,.06508089999999811],[\"University of Erlangen-Nuremberg\",49.5978804,11.004550699999982],[\"University of Florida\",29.6436325,-82.35493020000001],[\"University of Franche-Comté (Université de Franche-Comté)\",47.2405045,6.022618700000066],[\"University of Geneva\",40.7713156,-80.32184719999998],[\"University of Georgia\",33.9480053,-83.37732210000001],[\"University of Giessen\",50.58052,8.678020100000026],[\"University of Gothenburg\",57.6981719,11.97187800000006],[\"University of Gottingen (Georg-August-Universität Göttingen)\",51.5464725,9.943645699999934],[\"University of Granada (Universidad de Granada)\",37.1846223,-3.600632899999937],[\"University of Greifswald (Ernst-Moritz-Arndt-Universität Greifswald)\",54.095094,13.374605900000006],[\"University of Haifa\",32.7614296,35.01951840000004],[\"University of Hamburg (Universität Hamburg)\",53.5665641,9.984619500000008],[\"University of Hartford\",41.7985989,-72.71400019999999],[\"University of Hawaii - Manoa\",21.296939,-157.81711180000002],[\"University of Houston\",29.7199489,-95.3422334],[\"University of Hull\",53.7737034,-.3680781000000479],[\"University of Illinois at Chicago\",41.87044969999999,-87.66748539999998],[\"University of Illinois Urbana-Champaign\",40.1019523,-88.22716149999997],[\"University of Innsbruck (Universität Innsbruck)\",47.26335419999999,11.383800599999972],[\"University of Iowa\",41.6626963,-91.55489979999999],[\"University of Kansas\",38.9543439,-95.2557961],[\"University of Kent\",51.29846690000001,1.0709944000000178],[\"University of Kentucky\",38.0306511,-84.50396969999997],[\"University of Konstanz (Universität Konstanz)\",47.689426,9.186877699999968],[\"University of Leeds\",53.8066815,-1.5550327999999354],[\"University of Lille\",50.6283462,3.073962100000017],[\"University of Lincoln\",53.2279107,-.5501933000000463],[\"University of Lisbon\",38.7526578,-9.158244999999965],[\"University of Liverpool\",53.405936,-2.965572199999997],[\"University of Los Andes (Universidad de los Andes)\",4.6014855,-74.06644570000003],[\"University of Louisiana, Lafayette\",30.2114404,-92.02041209999999],[\"University of Louisville\",38.2122761,-85.75850229999998],[\"University of Malaya\",3.1201068,101.65454920000002],[\"University of Manchester\",53.4668498,-2.2338836999999785],[\"University of Manitoba\",49.8075008,-97.13662590000001],[\"University of Maryland\",38.9869183,-76.94255429999998],[\"University of Massachusetts, Amherst\",42.3911569,-72.5267121],[\"University of Massachusetts, Boston\",42.3148413,-71.0367377],[\"University of Massachusetts, Dartmouth\",41.6292932,-71.0061561],[\"University of Memphis\",35.118741,-89.937141],[\"University of Miami\",25.7904064,-80.21199279999996],[\"University of Michigan\",42.2780436,-83.73822410000002],[\"University of Minnesota\",44.97399,-93.22772850000001],[\"University of Missouri\",38.9403808,-92.32773750000001],[\"University of Missouri, Kansas City\",39.0335539,-94.57602589999999],[\"University of Missouri, St. Louis\",38.7092401,-90.30827999999997],[\"University of Mons\",50.4587714,3.9521651999999676],[\"University of Montana\",46.8600672,-113.98520810000002],[\"University of Montreal (Université de Montréal)\",45.5056156,-73.6137592],[\"University of Muenster (Münster)\",51.9635705,7.613182599999959],[\"University of Nantes\",47.2095499,-1.555971799999952],[\"University of Nebraska, Lincoln\",40.8201966,-96.70047629999999],[\"University of Nevada, Las Vegas\",36.1085197,-115.14317089999997],[\"University of New Hampshire\",43.138948,-70.9370252],[\"University of New Mexico\",35.0843187,-106.61978120000003],[\"University of New South Wales\",-33.917347,151.23126750000006],[\"University of Nicosia\",35.16582,33.314437999999996],[\"University of North Carolina, Asheville\",35.6162921,-82.5673127],[\"University of North Carolina, Chapel Hill\",35.9049122,-79.0469134],[\"University of North Carolina, Greensboro\",36.0689296,-79.81019750000002],[\"University of North Carolina, School of Medicine\",35.906058,-79.05219899999997],[\"University of North Carolina, Wilmington\",34.2239869,-77.87013250000001],[\"University of North Florida\",30.2661204,-81.50723140000002],[\"University of North Georgia\",34.5278618,-83.98444159999997],[\"University of Notre Dame\",41.7055716,-86.23533880000002],[\"University of Oklahoma\",35.2058936,-97.4457137],[\"University of Oregon\",44.0448302,-123.07260550000001],[\"University of Oslo\",59.9399586,10.721749600000066],[\"University of Otago\",-45.8646835,170.51442270000007],[\"University of Ottawa\",45.42310639999999,-75.68313289999998],[\"University of Padua (Università di Padova)\",45.406766,11.877446200000009],[\"University of Paris Nanterre\",48.90339900000001,2.211274000000003],[\"University of Pennsylvania\",39.9522188,-75.1932137],[\"University of Pittsburgh\",40.4443533,-79.96083499999997],[\"University of Poitiers (Université de Poitiers)\",46.5859908,.3460479000000305],[\"University of Portsmouth\",50.79512870000001,-1.093568600000026],[\"University of Puget Sound\",47.261764,-122.48145499999998],[\"University of Reading\",51.4414205,-.9418157000000065],[\"University of Redlands\",34.0630404,-117.16324259999999],[\"University of Regina\",50.41701339999999,-104.58848999999998],[\"University of Rennes (Université de Rennes)\",48.1159299,-1.6729599999999891],[\"University of Rochester\",43.1305531,-77.62600329999998],[\"University of Salford\",53.48458600000001,-2.2707550000000083],[\"University of São Paulo\",-23.5613991,-46.73078910000004],[\"University of Sheffield\",53.3809409,-1.4879468999999972],[\"University of South Brittany (Université de Bretagne Sud)\",48.3980356,-4.507641499999977],[\"University of South Carolina\",33.996112,-81.02742760000001],[\"University of South Dakota\",42.7883015,-96.92533809999998],[\"University of Southern California\",34.0223519,-118.28511700000001],[\"University of South Florida\",28.0587031,-82.41385389999999],[\"University of St Andrews\",56.3416934,-2.7927521999999954],[\"University of Stirling\",56.1459171,-3.918879000000061],[\"University of Surrey\",51.2421839,-.5905420999999933],[\"University of Sussex\",50.86708950000001,-.0879139999999552],[\"University of Tampere\",61.49374479999999,23.77873539999996],[\"University of Tennessee\",35.9544013,-83.92945639999999],[\"University of the Balearic Islands\",39.641222,2.6455590000000484],[\"University of the Basque Country\",43.3309433,-2.967892099999972],[\"University of the Incarnate Word\",29.4675939,-98.4676217],[\"University of the Pacific\",37.9808152,-121.31202250000001],[\"University of the Republic\",-34.9010589,-56.17333819999999],[\"University of the Sciences\",39.9468299,-75.20709790000001],[\"University of the West of England\",51.5001344,-2.5475301000000172],[\"University of Toledo\",41.6580307,-83.61407009999999],[\"University of Toronto\",43.6628917,-79.39565640000001],[\"University of Toulouse - Jean Jaurès (Université Toulouse - Jean Jaurès)\",43.5795225,1.4032247000000098],[\"University of Trento\",46.0667967,11.123116500000037],[\"University of Trier (Universität Trier)\",49.7457597,6.688469800000007],[\"University of Tuebingen (Tübingen)\",48.5294782,9.043773999999985],[\"University of Utah\",40.7649368,-111.84210210000003],[\"University of Verona\",45.43739799999999,11.003376000000003],[\"University of Virginia\",38.0335529,-78.50797720000003],[\"University of Warsaw\",52.2403463,21.018601200000035],[\"University of Washington\",47.65533509999999,-122.30351989999997],[\"University of Waterloo\",43.4722854,-80.5448576],[\"University of Western Sydney\",-33.8757124,151.20920360000002],[\"University of Wisconsin, Green Bay\",44.53207339999999,-87.92171429999996],[\"University of Wisconsin, Madison\",43.076592,-89.4124875],[\"University of Wisconsin, Milwaukee\",43.078263,-87.8819686],[\"University of Wisconsin, Oshkosh\",44.0253186,-88.55102149999999],[\"University of Wisconsin, Whitewater\",42.8411986,-88.74308450000001],[\"University of Wuerzburg (University of Würzburg )\",49.7830083,9.970846199999983],[\"University of York\",53.9455334,-1.0561666999999488],[\"University of Zurich (Universität Zürich)\",47.3743221,8.550981200000024],[\"University Paris 8\",48.9449361,2.363562199999933],[\"University Saint Anne (Université Sainte-Anne)\",45.4077736,-73.93882209999998],[\"Uppsala University\",59.85090049999999,17.630009299999983],[\"Utrecht University\",52.0901527,5.122601799999984],[\"Vanderbilt University\",36.1447034,-86.80265509999998],[\"Vietnam Maritime University\",20.8368539,106.69420869999999],[\"Villanova University\",40.037056,-75.34357999999997],[\"Virginia Commonwealth University\",37.5488396,-77.45272720000003],[\"Virginia Military Institute\",37.788981,-79.43946],[\"Virginia Tech\",37.22838429999999,-80.42341669999996],[\"Vrije Universiteit Brussel\",50.821658,4.394886000000042],[\"Wabash College\",40.0378111,-86.90665519999999],[\"Wake Forest University\",36.1352495,-80.2763425],[\"Washington State University\",46.7319225,-117.1542121],[\"Washington University, St Louis\",38.6487895,-90.31079620000003],[\"Wayne State University\",42.3591388,-83.0665462],[\"Weill Cornell Medical College\",40.7649911,-73.95478989999998],[\"Wellesley College\",42.2935733,-71.30592769999998],[\"Wesleyan University\",41.5566104,-72.65690410000002],[\"Westmont College\",34.4488434,-119.66103129999999],[\"West Virginia University\",39.6361396,-79.95593580000002],[\"Wheaton College\",41.86833,-88.09962200000001],[\"Whitman College\",46.07057990000001,-118.3306351],[\"Widener University\",39.8633562,-75.3566753],[\"Wilfrid Laurier University\",43.4723532,-80.52633989999998],[\"Williams College\",42.7128038,-73.20302140000001],[\"Worcester Polytechnic Institute\",42.27463729999999,-71.80633899999998],[\"Xidian University\",34.125346,108.83885899999996],[\"Yale University\",41.3163244,-72.92234309999998],[\"York University\",43.7734535,-79.50186839999998]],o=new google.maps.InfoWindow;for(t=0;t<r.length;t++){var s=new google.maps.LatLng(r[t][1],r[t][2]);e.extend(s),n=new google.maps.Marker({position:s,map:i,title:r[t][0]}),google.maps.event.addListener(n,\"mouseover\",function(e,n){return function(){o.setContent('<div class=\"info_content\">'+r[n][0]+\"</div>\"),o.open(i,e)}}(n,t)),i.fitBounds(e)}var a=google.maps.event.addListener(i,\"bounds_changed\",function(i){this.setCenter(new google.maps.LatLng(24.215527,-12.885834)),this.setZoom(3),google.maps.event.removeListener(a)})}"
  where
  title =
#ifdef SANDBOX
    "Databrary Demo"
#else
    "Databrary"
#endif
