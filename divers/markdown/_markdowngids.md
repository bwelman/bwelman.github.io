# Markdown Tutorial: Van WYSIWYG naar Platte Tekst

## Inleiding

Als je gewend bent aan Microsoft Word, LibreOffice Writer of Google Docs, lijkt het misschien vreemd om tekst te gaan opmaken met symbolen en leestekens in plaats van knoppen en menu's. Toch is Markdown – een eenvoudige opmaaktaal voor platte tekst – steeds populairder geworden, niet alleen bij programmeurs maar ook bij schrijvers, studenten en professionals. In deze tutorial ontdek je wat Markdown is, waarom het de moeite waard is om te leren, en wanneer je het het beste kunt gebruiken.

## Wat is Markdown?

Markdown is een eenvoudige manier om tekst op te maken met gewone tekens en symbolen. In plaats van tekst te selecteren en op een knop "Vet" te klikken, zet je bijvoorbeeld sterretjes om de tekst: `**dit wordt vet**`. Het resultaat ziet er dan zo uit: **dit wordt vet**.

Het belangrijkste principe: je schrijft in platte tekst (zoals in Kladblok of Notities), maar door simpele symbolen toe te voegen kun je aangeven hoe de tekst eruit moet zien wanneer hij wordt weergegeven.

## Waarom Markdown? De voordelen van platte tekst

### 1. Simpel en snel

Je hoeft niet voortdurend met je muis naar knoppen te zoeken. Als je eenmaal de basisregels kent, typ je sneller dan in een WYSIWYG-editor. Je handen blijven op het toetsenbord.

### 2. Overal hetzelfde

Een Markdown-bestand kun je openen in:

-   Windows Kladblok
-   macOS TextEdit
-   Je smartphone (Notes, Joplin, Obsidian)
-   Een terminal op een server
-   Elke teksteditor die je maar kunt bedenken

Je bent niet afhankelijk van specifieke software. Een `.docx` bestand heeft Word nodig (of een compatibel programma), maar een `.md` bestand kun je overal openen.

### 3. Klein en licht

Platte tekstbestanden zijn extreem klein. Een document van 50 pagina's in Markdown is misschien 50 KB, terwijl hetzelfde document in Word al snel 500 KB of meer kan zijn. Ideaal voor synchroniseren via de cloud of opslaan op je smartphone.

### 4. Versiebeheer

Als je met Git of andere versiebeheersystemen werkt, kun je precies zien welke regels zijn veranderd. Bij Word-documenten zie je alleen dat "het bestand is gewijzigd", maar niet exact wat er anders is.

### 5. Focus op inhoud

Je wordt niet afgeleid door lettertypen, kleuren en lay-out. Je richt je op wat je schrijft, niet op hoe het eruitziet. De opmaak komt later, of wordt automatisch toegepast door de Markdown-viewer.

### 6. Toekomstbestendig

Platte tekst blijft altijd leesbaar. Over 20 jaar kun je een `.md` bestand nog steeds probleemloos openen. Kun je dat ook zeggen van een `.docx` bestand gemaakt in Word 2005?

## Wanneer is Markdown NIET geschikt?

Wees eerlijk: Markdown is niet voor alles de beste keuze.

### Complexe lay-out

Als je een boek of scriptie schrijft met:

-   Precieze paginamarges en opmaak
-   Meerdere kolommen
-   Complexe tabelstructuren met samengevoegde cellen
-   Verschillende kopstijlen door het document
-   Nauwkeurige typografie

Dan is een professionele DTP-programma of LaTeX een betere keuze. Markdown houdt het bewust simpel.

### Zakelijke documenten met strikte huisstijl

Voor offertes, contracten of officiële brieven met bedrijfslogo's, handtekeningen en nauwkeurige lay-out is een WYSIWYG-editor vaak praktischer. Je hebt volledige controle over elk pixel.

### Samenwerken met mensen die Markdown niet kennen

Als je collega's of docenten verwachten dat je documenten in Word of PDF aanlevert, kun je extra werk krijgen door vanuit Markdown te werken (hoewel conversie mogelijk is).

### Veel afbeeldingen met tekst eromheen

Afbeeldingen naast tekst of tekstomloop om afbeeldingen is in Markdown beperkt mogelijk. Dit kan met HTML, maar dan verlies je de eenvoud van Markdown.

## Wanneer is Markdown WEL ideaal?

### Notities en documentatie

Voor dagelijkse notities, meeting notes, persoonlijke wiki's of technische documentatie is Markdown perfect. Snel, overzichtelijk en gemakkelijk doorzoekbaar.

### Online content

Veel websites, blogs en platforms (GitHub, Reddit, Stack Overflow) gebruiken Markdown. Als je content voor het web schrijft, is Markdown een logische keuze.

### Academisch schrijven (met beperkingen)

Voor essays, scripties en papers werkt Markdown prima, vooral in combinatie met tools als Pandoc om naar PDF, Word of LaTeX te converteren. Let wel: check eerst de eisen van je opleiding.

### Op je smartphone of tablet

Op een klein scherm zijn WYSIWYG-editors vaak omslachtig. Markdown-apps zijn licht en snel, perfect voor onderweg notities maken.

### Samenwerken via Git

Als je team al Git gebruikt (vaak bij software-ontwikkeling), is Markdown ideaal voor documentatie. Je kunt samen werken, wijzigingen volgen en conflicten oplossen.

## De leercurve: hoe moeilijk is Markdown?

**Goed nieuws: Markdown is eenvoudig!**

### Fase 1: De eerste 15 minuten (Basis)

Je leert:

-   Koppen maken met `#`
-   Tekst **vet** maken met `**`
-   Tekst *cursief* maken met `*`
-   Lijsten maken met `-` of cijfers
-   Links toevoegen met `[tekst](url)`

Dit is genoeg voor 80% van je dagelijkse gebruik. Serieus: binnen een kwartier kun je nuttige Markdown schrijven.

### Fase 2: Het eerste uur (Gevorderd)

Je ontdekt:

-   Citaten met `>`
-   Code-blokken met \`\`\` of inspringen
-   Afbeeldingen invoegen met `![alt-tekst](pad)`
-   Tabellen maken
-   Horizontale lijnen

Na een uur experimenteren beheers je de meeste Markdown-mogelijkheden.

### Fase 3: Doorlopend leren (Expert)

Je duikt in:

-   Speciale syntax van je gekozen Markdown-variant
-   Geavanceerde trucs met HTML (als je dat wilt)
-   Automatisering en workflows

Maar dit is optioneel. De meeste mensen blijven prima functioneren met wat ze in fase 1 en 2 leerden.

**Vergelijking:** Leren werken met Word kost ook tijd, denk aan alle menu's, opties, stijlen en functies. Bij Markdown leer je de basis veel sneller, en daarna kun je meteen productief zijn.

## Verschillende Markdown-varianten: is dit een probleem?

Ja, er zijn verschillende "smaken" van Markdown:

-   [CommonMark](https://commonmark.org/help/): De gestandaardiseerde versie, met duidelijke specificaties.
-   [GitHub Flavored Markdown](https://docs.github.com/en/get-started/writing-on-github/getting-started-with-writing-and-formatting-on-github/basic-writing-and-formatting-syntax): Uitbreiding voor GitHub, met taaklijsten en tabellen.
-   Markdown Extra: Toevoegingen zoals voetnoten en definities.
-   MultiMarkdown: Nog meer functies, zoals metadata.

### Is dit verwarrend?

Niet echt! Hier is waarom:

1.  **De basis is overal hetzelfde**: Alle varianten ondersteunen koppen, vet, cursief, lijsten en links. Dit is 90% van wat je gebruikt.

2.  **Verschillen zijn kleine uitbreidingen**: De varianten voegen features toe, maar veranderen de basis niet. Een document in standaard Markdown werkt in alle systemen.

3.  **Je merkt het pas bij geavanceerde functies**: Als je tabellen, taaklijsten of voetnoten wilt gebruiken, moet je even checken of jouw tool dit ondersteunt. Maar voor dagelijks gebruik maakt het nauwelijks uit.

### Praktisch advies

Begin met CommonMark of GitHub Flavored Markdown (GFM). Deze werken op de meeste platforms. Als je later specifieke features nodig hebt, kun je altijd uitbreiden.

## Welke editor moet je gebruiken?

### Optie 1: Gewone teksteditor (Kladblok, TextEdit, Notepad++)

**Voordelen:**

-   Geen extra software nodig
-   Uiterst eenvoudig
-   Geen afleiding

**Nadelen:**

-   Geen preview van hoe het eruit ziet
-   Geen syntax highlighting
-   Geen hulp bij de Markdown-syntax

**Advies:** Je zou hiermee kunnen beginnen, maar omdat je niet ziet hoe het eruit gaat zien wil je waarschijnlijk iets beters. Wanneer je geen sofware wilt installeren en toch Markdown wilt leren, kun je beter beginnen met een van de vele online editors zoals

-   [CommonMark Tutorial](https://commonmark.org/help/): Bevat een interactieve tutorial met opdrachten. Zeer geschikt voor het leren van Markdown. Je kunt er geen eigen bestanden mee maken en opslaan.
-   [Dillinger](https://dillinger.io): Een eenvoudige online Markdown-editor met preview. Je kunt een eigen document maken en deze dan exporteren in markdown en pdf formaat.

### Optie 2: Markdown-specifieke editor (Obsidian, MarkText, Typora)

**Voordelen:**

-   Live preview of WYSIWYG-modus (je ziet meteen het resultaat)
-   Syntax highlighting
-   Hulp bij invoegen van afbeeldingen, links, tabellen
-   Vaak extra functies zoals tags, backlinks, grafieken

**Nadelen:**

-   Je moet software installeren
-   Kan overkill zijn voor simpele notities
-   Elke editor heeft eigen extra features (kan verwarrend zijn)

**Advies:** Dit is voor de meeste mensen de beste keuze. MarkText is een zeer eenvoudige gratis editor. Typora (betaald maar goedkoop) is gebruiksvriendelijker. Obsidian (gratis voor normaal gebruik) is uitstekend en aan te bevelen wanneer je ook jouw Markdown notities wilt beheren. Alledrie zijn beschikbaar voor macOS, Linux en Windows. Obsidian is er ook voor Android.

### Optie 3: Code-editor met Markdown-ondersteuning (VS Code)

**Voordelen:**

-   Krachtig en flexibel
-   Preview beschikbaar via extensies
-   Ideaal als je ook programmeert

**Nadelen:**

-   Steile leercurve voor niet-programmeurs
-   Veel functies die je niet nodig hebt

**Advies:** Alleen als je al een code-editor gebruikt voor programmeren.

### Mijn aanbeveling

Start met een Markdown-specifieke editor met preview. Obsidian of Typora zijn uitstekende keuzes voor beginners. Ze laten je meteen zien wat je krijgt, maar houden de eenvoud van Markdown intact.

## De belangrijkste Markdown-opmaak

Hier is een compleet overzicht van wat je het meest zult gebruiken:

### Koppen

``` markdown
# Hoofdkop (Heading 1)
## Tweede niveau (Heading 2)
### Derde niveau (Heading 3)
#### Vierde niveau (Heading 4)
##### Vijfde niveau (Heading 5)
###### Zesde niveau (Heading 6)
```

**Let op:** Zet altijd een spatie na de `#` tekens.

### Vet en cursief

``` markdown
**Deze tekst is vet**
*Deze tekst is cursief*
***Deze tekst is vet én cursief***
```

**Alternatief:** Je kunt ook underscores gebruiken: `__vet__` en `_cursief_`, maar sterretjes zijn gebruikelijker.

### Lijsten

**Ongeordende lijst (bullets):**

``` markdown
- Eerste item
- Tweede item
- Derde item
  - Sub-item (inspringen met 2 spaties)
  - Nog een sub-item
- Vierde item
```

**Geordende lijst (nummers):**

``` markdown
1. Eerste stap
2. Tweede stap
3. Derde stap
   1. Sub-stap (inspringen met 3 spaties)
   2. Nog een sub-stap
4. Vierde stap
```

**Tip:** Bij genummerde lijsten kun je overal `1.` gebruiken – Markdown nummert automatisch door:

``` markdown
1. Eerste
1. Tweede
1. Derde
```

Dit wordt automatisch 1, 2, 3. Handig als je items toevoegt of verwijdert!

### Links

``` markdown
[Tekst die zichtbaar is](https://www.voorbeeld.nl)
[MijnOverheid](https://mijn.overheid.nl/)
```

**Directe URL zichtbaar:**

``` markdown
<https://www.knvb.nl/>
```

### Afbeeldingen

``` markdown
![Alt-tekst voor de afbeelding](pad/naar/afbeelding.jpg)
![Logo van bedrijf](logo.png)
```

**Let op:** Dit is bijna hetzelfde als een link, maar met een `!` ervoor.

### Citaten

``` markdown
> Dit is een citaat.
> Het kan over meerdere regels lopen.
>
> En zelfs meerdere alinea's bevatten.
```

### Code

**Inline code (in een zin):**

``` markdown
Gebruik de functie `print()` om iets af te drukken.
```

**Code-blok:** \~\~\~markdown

``` python
def welkom():
    print("Hallo wereld!")
```

\~\~\~

**Let op:** Je kunt de programmeertaal specificeren (zoals `python`, `javascript`, `r`) voor syntax highlighting.

### Horizontale lijn

``` markdown
---
```

Of:

``` markdown
***
```

### Tabellen (GitHub Flavored Markdown)

``` markdown
| Kop 1     | Kop 2     | Kop 3     |
|-----------|-----------|-----------|
| Rij 1, A  | Rij 1, B  | Rij 1, C  |
| Rij 2, A  | Rij 2, B  | Rij 2, C  |
```

**Uitlijning aangeven:**

``` markdown
| Links     | Midden    | Rechts    |
|:----------|:---------:|----------:|
| Links     | Midden    | Rechts    |
```

**Let op:** Tabellen zijn niet in alle Markdown-varianten beschikbaar.

### Taaklijsten (GitHub Flavored Markdown)

``` markdown
- [x] Afgeronde taak
- [ ] Open taak
- [ ] Nog een open taak
```

### Voetnoten (niet overal ondersteund)

``` markdown
Hier is een zin met een voetnoot.[^1]

[^1]: Dit is de voetnoot tekst.
```

## Wat is lastig of moeilijk?

### **1.** Afbeeldingen positioneren

In Word kun je een afbeelding precies plaatsen en tekst eromheen laten lopen. In Markdown is dit niet standaard mogelijk. Afbeeldingen staan op hun eigen regel.

**Oplossing:** Gebruik HTML als je meer controle nodig hebt, of accepteer de beperkingen.

### **2.** Complexe tabellen

Tabellen met samengevoegde cellen of complexe structuren zijn lastig in Markdown. De syntax wordt al snel onoverzichtelijk.

**Oplossing:** Voor simpele tabellen is Markdown prima. Voor complexe tabellen: gebruik HTML of een andere tool.

### **3.** Verschillende renderingen

Hetzelfde Markdown-bestand kan er op verschillende platforms anders uitzien. Lettertypen, kleuren en marges worden bepaald door de viewer, niet door jou.

**Oplossing:** Accepteer dat je geen pixelperfecte controle hebt. Als dat wel moet, is Markdown niet de juiste keuze.

### **4.** Lege regels en witruimte

Markdown is gevoelig voor lege regels. Twee enters tussen alinea's, één enter binnen een alinea. Dit kan verwarrend zijn.

**Voorbeeld:**

``` markdown
Dit is alinea 1.
Dit staat ook in alinea 1 (één enter).

Dit is alinea 2 (twee enters).
```

**Oplossing:** Wennen. Na een paar documenten wordt dit automatisme.

### **5.** Speciale tekens

Sommige tekens hebben een betekenis in Markdown (`*`, `_`, `#`, `[`, etc.). Als je deze letterlijk wilt typen, moet je ze "escapen" met een backslash: `\*`.

**Voorbeeld:**

``` markdown
\*Dit wordt geen cursief\*
```

**Oplossing:** De meeste Markdown-editors helpen je hierbij, en in de praktijk kom je dit zelden tegen.

## Tips voor beginners

1.  **Begin klein:** Maak eerst alleen gebruik van koppen, vet, cursief en lijsten. Dit is genoeg voor 90% van je notities.

2.  **Gebruik een preview:** Kies een editor met live preview, zodat je meteen ziet wat je krijgt. Dit versnelt het leerproces enorm.

3.  **Bekijk voorbeelden:** GitHub is vol met Markdown-bestanden (zoals README.md files). Bekijk de broncode om te zien hoe anderen Markdown gebruiken.

4.  **Maak een cheatsheet:** Print een Markdown-overzicht en leg het naast je toetsenbord. Na een paar dagen heb je het uit je hoofd.

5.  **Wees niet bang voor fouten:** Verkeerde Markdown-syntax breekt niets. In het ergste geval zie je letterlijke sterretjes of hekjes in je tekst. Corrigeer het en ga door.

6.  **Combineer met andere tools:** Markdown kun je gemakkelijk converteren naar Word, PDF, HTML of zelfs presentaties (met Pandoc of andere tools). Je hoeft niet voor altijd in Markdown te blijven.

## Conclusie: moet jij overstappen op Markdown?

Markdown is niet de heilige graal voor alle tekstverwerkingsbehoeften, maar het is een waardevol gereedschap in je digitale toolbox.

**Je zult Markdown waarderen als je:**

-   Snel notities wilt maken zonder afgeleid te worden
-   Documenten wilt die overal werken (laptop, tablet, telefoon)
-   Content schrijft voor het web
-   Samenwerkt via Git of andere versiebeheersystemen
-   Kleine, toekomstbestendige bestanden wilt

**Je houdt waarschijnlijk Word of LibreOffice als je:**

-   Complexe documenten met precieze lay-out maakt
-   Zakelijke documenten met strikte huisstijl moet leveren
-   Intensief samenwerkt met mensen die WYSIWYG-editors gebruiken
-   Veel afbeeldingen met tekst eromheen moet plaatsen

**Het mooie nieuws:** Je hoeft niet te kiezen! Gebruik Markdown voor notities, documentatie en snelle teksten, en WYSIWYG-editors voor formele documenten en complexe lay-outs. Beide hebben hun plaats.

Probeer Markdown een week uit voor je dagelijkse notities. Download Obsidian of Typora, maak een paar documenten, en ervaar zelf of de eenvoud en snelheid je aanspreken. De leercurve is laag, de voordelen zijn groot, en je kunt altijd terug naar je oude werkwijze als het niet bevalt.

De [Markdown Guide](https://www.markdownguide.org/) is een online **naslagwerk** dat uitlegt hoe je Markdown kunt gebruiken en is een handige bron om verder te leren.

**Veel succes met je Markdown-avontuur!**
