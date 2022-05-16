## remotes::install_github('ccs-amsterdam/ccsAnnotator')
library(ccsAnnotator)

## codebook variables
geloofwaardig = question('geloofwaardig', 'Vind jij de inhoud van dit bericht geloofwaardig?', type='annotinder',
                         codes = c(Nee = 'crimson', Ja = 'lightgreen'))

## unit data
d = data.frame(stimulus = list.files('~/projects/swipe_right/data/stimulus', pattern='\\.jpg', full.names = T))
d$set = gsub('_.*', '', gsub('.*/Set', '', d$stimulus) )
d$id = gsub('\\..*', '', gsub('.*/', '', d$stimulus) )

## split units into three jobsets
jobsets = list(
  jobset('Set_1', unit_set = grep('Set1', d$id, value = T)),
  jobset('Set_2', unit_set = grep('Set2', d$id, value = T)),
  jobset('Set_3', unit_set = grep('Set3', d$id, value = T)),
  jobset('Set_4', unit_set = grep('Set4', d$id, value = T)),
  jobset('Set_5', unit_set = grep('Set5', d$id, value = T)),
  jobset('Set_6', unit_set = grep('Set6', d$id, value = T))
)

## pre / post questions

intro_markdown = '
# Introductie
Hartelijk dank voor uw deelname aan dit onderzoek.

Voordat het onderzoek begint, is het belangrijk dat u op de hoogte bent van de procedure van dit onderzoek. Lees daarom onderstaande tekst zorgvuldig door. Mocht de tekst niet duidelijk zijn, dan zijn wij als onderzoekers via de onderstaande e-mailadressen bereikbaar om eventuele vragen te beantwoorden.

## Doel en duur van het onderzoek
Het doel van het onderzoek is om meer inzicht te krijgen in de nieuwsconsumptie in Nederland. Het onderzoek bestaat uit drie losstaande onderdelen. In het eerste deel wordt u gevraagd om aan aantal vragen wat betreft uw nieuwsconsumptie en persoonlijke kenmerken te beantwoorden. In het tweede deel volgen een aantal nieuwsberichten en vragen wij u deze op de mate van geloofwaardigheid te beoordelen. Hier volgt later in het onderzoek meer informatie over. Na het beoordelen van de nieuwsberichten beantwoordt u nog enkele vragen ter afsluiting van het onderzoek.

Het onderzoek zal ongeveer 10 minuten van uw tijd in beslag nemen. Na het invullen van de vragenlijst maakt u kans op een **cadeaubon van Bol.com ter waarde van €20 euro**.

## Vertrouwelijkheid van gegevens
Uw antwoorden op de vragen zijn anoniem, vertrouwelijk en zullen uitsluitend voor wetenschappelijke doeleinden gebruikt worden. U kunt op elk moment afzien van deelname zonder opgave van reden.

## Nadere inlichtingen
Mocht u nog vragen hebben over dit onderzoek dan kunt u zich wenden tot de onderzoekers:

* Romijn Hormann, **e-mail**: [c.r.hormann@student.vu.nl](mailto:c.r.hormann@student.vu.nl)
* Willemijn Bierhuizen, **e-mail**: [w.a.m.bierhuizen@student.vu.nl](mailto:w.a.m.bierhuizen@student.vu.nl)
* Rosa Schilder, **e-mail**: [r3.schilder@student.vu.nl](mailto:r3.schilder@student.vu.nl)
'

intro = create_question_unit(id="introductie", markdown=intro_markdown,
    question('consent', 'Informed consent', codes="Ik heb bovenstaande informatie gelezen en begrepen, en geef toestemming voor deelname aan het onderzoek en gebruik van de daarmee verkregen gegevens"),
    question('leeftijd', "Wat is je leeftijd?", type="inputs", items=list(name = list(type='number', label='leeftijd'))),
    question('geslacht', 'Wat is je geslacht?', codes = c('man','vrouw','anders','zeg ik liever niet')),
    question('opleiding', 'Wat is je hoogst genoten opleiding?', codes = c('Middelbare school','VMBO','HAVO','VWO','MBO','HBO','WO','WO Master')))


trust = create_question_unit(id='vertrouwen', title='', text='', text_window_size = 'auto',
    question('vertrouwen', 'Hieronder ziet u een aantal instituties. Kunt u aangeven hoeveel vertrouwen u heeft in deze instituties?', type='scale',
            codes = c('totaal geen vertrouwen', 'een beetje vertrouwen','best wel veel vertrouwen','veel vertrouwen'),
            items=c(V1_regering='vertrouwen in regering',
                    V2_overheidsinstanties='vertrouwen in overheidsinstanties (denk aan gemeentes, organisaties zoals het UWV en de politie)',
                    V3_EU='vertrouwen in Europese Unie',
                    V4_nieuwsmedia='vertrouwen in Nederlandse nieuwsmedia')))

social = create_question_unit(id="instagram", title='Instagram',
    question('account', 'Heb je een instagram account?', codes=list(code('Nee', color='crimson', makes_irrelevant='REMAINING'),
                                                                    code('Ja', color='lightgreen'))),
    question('frequentie', "Hoe vaak check je je instagram?", codes = c("Bijna nooit","Eens in de paar dagen","Een keer per dag","Meerdere keren per dag","Elk uur")),
    question('tijd', 'Hoe veel tid spendeer je op Instagram op een gemiddelde dag?', codes = c('5 minuten of minder','15 minuten','30 minuten','1 uur','2 uur','3 uur','meer dan 3 uur','meer dan 5 uur')))


nieuws = create_question_unit(id='nieuws', title='Nieuwsconsumptie', text='',
    question('nieuwsbron', 'Via welke media verkrijg jij normaal gesproken meestal je nieuws?',
             codes = list('Papieren krant','Televisie','Radio','Online nieuws site','Social media','Ik lees geen nieuws',
                          code('Anders, namelijk', required_for = 'nieuwsbron_anders'))),
    question('nieuwsbron_anders', 'Via welke media verkrijg jij normaal gesproken meestal je nieuws?', type="inputs", items=list(media = list(type='textarea', label=''))))

insta_nieuws = create_question_unit(id='insta_nieuws', title='Nieuwsconsumptie', text='', text_window_size=20,
     question('instagram_nieuws', 'Geef van de volgende stellingen aan in hoeverre deze van toepassing zijn voor jou', type='scale',
              codes = c('sterk mee oneens','oneens','een beetje oneens','neutraal','een beetje eens','eens','sterk mee eens'),
              items=list(V1_gebruik='Ik gebruik Instagram voor mijn nieuwsconsumptie',
                         V2_exposure='Ik kom vaak nieuws tegen op Instagram',
                         V3_volgen='Ik volg Instagram accounts die mij van nieuws voorzien',
                         V4_vergeleken_met='Vergeleken met andere kanalen gebruik ik vooral Instagram voor mijn nieuwsconsumptie')))


swipe_markdown = '
# Geloofwaardigheid van nieuwsberichten

In het volgende onderdeel krijgt u een aantal nieuwsberichten te zien. U wordt gevraagd in hoeverre u de artikelen geloofwaardig vindt.

Indien u via uw mobiele telefoon meedoet aan het onderzoek, kunt u na het zien van een nieuwsartikel naar links swipen of naar rechts swipen.
Swipe naar links (veeg met uw vinger naar links) wanneer u het artikel **niet geloofwaardig vindt**.
Swipe naar rechts (veeg met uw vinger naar rechts) wanneer u het artikel **wel geloofwaardig vindt**.
Als u via uw laptop of computer meedoet aan het onderzoek, ziet u onderaan het nieuwsartikel de antwoordmogelijkheden.
'

swipeinfo = create_info_unit(id='swipe', markdown=swipe_markdown, button='Ik heb de instructies begrepen')


post = create_question_unit(id='post_survey', title='Laatste vraag!', text="We zijn bijna op het einde aangekomen van dit onderzoek. Er volgen nu nog een aantal afsluitende vragen",
    text_window_size=20,
    question('hedonistic', 'Geef van de volgende stellingen aan in hoeverre deze van toepassing zijn voor jou', type='scale',
             codes = c('sterk mee oneens','oneens','een beetje oneens','neutraal','een beetje eens','eens','sterk mee eens'),
             items=c(V1_vermakelijk='Swipen door de Instagram nieuws content voelde vermakelijk',
                     V2_wilde='Ik ging niet door met swipen omdat het moest, maar omdat ik het wilde',
                     V3_plezier='Vergeleken met andere dingen die ik kon doen, had ik plezier tijdens het swipen',
                     V4_opwelling='Ik vermaakte mijzelf, omdat ik in de opwelling van het moment kon handelen',
                     V5_amuseren='Ik ervaarde het swipen door de Instagram nieuws content zoals ik me normaal zou amuseren op Instagram')),
    question('believability', 'In hoeverre denkt u dat de volgende uitspraken waar zijn?', type='scale',
             codes = c('zeker niet waar', 'waarschijnlijk niet waar','misschien niet waar','kan waar of niet waar zijn','misschien wel waar','waarschijnlijk wel waar','zeker wel waar'),
             items=c(V1_vliegtuig='"Passagiersvliegtuig met 132 inzittenden neergestort in China"',
                     V2_oekraine='"Hoogste marineofficier van Rusland gedood door Oekraïnse troepen"',
                     V3_dodenlijst='"VS: Rusland heeft al dodenlijst gemaakt"',
                     V4_gas_en_olie='"Kabinet wil voor eind van het jaar van Russische gas en olie af"')))


afsluiting_markdown = '
# Afsluiting


Hierbij bent u aan het einde gekomen van de vragen. Bedankt voor uw deelname aan dit onderzoek!

Als u kans wilt maken op een Bol.com cadeaubon ter waarde van €20 euro, kunt u hier uw e-mailadres achterlaten:

Graag willen we u nog attenderen op het feit dat alhoewel de getoonde nieuwstitels afkomstig zijn van nieuwsbronnen, dit geen garantie biedt voor hun betrouwbaarheid.
Neem de informatie uit de nieuwstitels van dit onderzoek dus niet voor feiten aan.

Advies is om altijd goed uit te zoeken of informatie valide is voordat u deze als waarheid aanneemt.

## Nadere inlichtingen
Mocht u nog verdere vragen hebben m.b.t. dit onderzoek, kunt u contact opnemen met:

* Romijn Hormann, **e-mail**: [c.r.hormann@student.vu.nl](mailto:c.r.hormann@student.vu.nl)
* Willemijn Bierhuizen, **e-mail**: [w.a.m.bierhuizen@student.vu.nl](mailto:w.a.m.bierhuizen@student.vu.nl)
* Rosa Schilder, **e-mail**: [r3.schilder@student.vu.nl](mailto:r3.schilder@student.vu.nl)
'

afsluiting = create_question_unit(id='email', markdown=afsluiting_markdown,
                                  question('email', 'Voer hier uw email adress in (optioneel)', type="inputs", items=list(name = list(type='email', label='email', optional=TRUE))))



## upload job
backend_connect('http://localhost:5001', 'test@user.com')
#backend_connect('https://kasperwelbers.com/annotator', 'test@user.com')

upload_job('Swipe right! versie 3',
           units=create_units(d, id='id', image='stimulus'),
           codebook=create_codebook(geloofwaardig),
           jobsets=jobsets,
           debrief=debrief('Bedankt voor het deelnemen aan dit onderzoek!'),
           pre=list(intro, trust, social, nieuws, insta_nieuws, swipeinfo),
           post=list(post, afsluiting))
