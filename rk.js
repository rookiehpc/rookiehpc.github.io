const RK = {
    BASE_URL: "",
    CONTACT_EMAIL: "contact@rookiehpc.com",
    LOOKUP_NAME: "lookup.json",
    ENTRY_TYPES: {
        TEXT: "Text",
        ABOUT: "About",
        DOCUMENTATION: "Documentation",
        EXERCISE: "Exercise",
        TOOL: "Tool",
        DOCUMENTATION_HOMEPAGE: "DocumentationHomepage",
        TOOL_HOMEPAGE: "ToolHomepage",
        EXERCISE_HOMEPAGE: "ExerciseHomepage",
        HOMEPAGE: "Homepage"
    },
    TECHNOLOGIES: {
        MPI: "MPI", 
        OPENMP: "OpenMP"
    },
    LANGUAGES: {
        C: "C",
        CPP: "C++",
        F90: "FORTRAN-90",
        F08: "FORTRAN-2008"
    },
    LANGUAGE_HLJS_CLASS: {
        C: "c",
        CPP: "cpp",
        F90: "f90",
        F08: "fortran"
    },
    LANGUAGE_FILE_EXTENSIONS: {
        C: "c",
        CPP: "cpp",
        F90: "f90",
        F08: "f08"
    },
    INTENTS: {
        IN: "IN",
        OUT: "OUT",
        INOUT: "INOUT"
    },
    DIFFICULTY: {
        0: "Rookie",
        1: "Easy",
        2: "Medium",
        3: "Hard"
    },

    ParametersPN: "Parameters",
    ParameterPN: "Parameter",
    PrototypePN: "Prototype",
    DimensionNumberPN: "DimensionNumber",
    NamePN: "Name",
    DescriptionPN: "Description",
    SharedDescriptionPN: "SharedDescription",
    IntroductionPN: "Introduction",
    HintsPN: "Hints",
    TypePN: "Type",
    LanguagesPN: "Languages",
    LanguagePN: "Language",
    ReturnPN: "Return",
    TechnologyPN: "Technology",
    TechnologiesPN: "Technologies",
    CategoriesPN: "Categories",
    OptionalPN: "Optional",
    IntentPN: "Intent",
    EntriesPN: "Entries",
    DirectoryNamePN: "DirectoryName",
    SectionsPN: "Sections",
    TitlePN: "Title",
    ContentPN: "Content",
    ToolsPN: "Tools",
    UrlPN: "Url",
    ImageUrlPN: "ImageUrl",
    DifficultyPN: "Difficulty",
    ExercisesPN: "Exercises",
    SolutionPN: "Solution",
    ProvidedPN: "Provided",

    ParameterTypeSplitter: '\t',
    LocationRoot: "Root",
    LocationSeparator: " > ",
    LocationOpener: "[",
    LocationCloser: "] ",
    Location: "Root",
    OutputElement: null,
    EditorMode: false,

    PathToMe: window.location.pathname,
    FolderPath: window.location.pathname.slice(0, window.location.pathname.lastIndexOf('/')),

    SetLocation: (Path) => {
        RK.Location = Path;
    },

    GetLocation: () => {
        return RK.Location;
    },

    SetOutput: (Output) => {
        RK.OutputElement = Output;
    },

    GetOutput: () => {
        return RK.OutputElement;
    },

    SetEditorMode: (Mode) => {
        RK.EditorMode = Mode;
    },

    IsEditorMode: () => {
        return RK.EditorMode;
    },

    IsParameterTypeRequired: (Language) => {
        switch(Language) {
            case RK.LANGUAGES.C:
                return true;
                break;
            case RK.LANGUAGES.CPP:
                return true;
                break;
            case RK.LANGUAGES.F90:
                return true;
                break;
            case RK.LANGUAGES.F08:
                return true;
                break;
            default:
                throw 'Language ' + Language + ' is not covered';
                break;
        }
    },

    CanParameterBeOptional: (Language) => {
        switch(Language) {
            case RK.LANGUAGES.C:
                return false;
                break;
            case RK.LANGUAGES.CPP:
                return false;
                break;
            case RK.LANGUAGES.F90:
                return false;
                break;
            case RK.LANGUAGES.F08:
                return true;
                break;
            default:
                throw 'Language ' + Language + ' is not covered';
                break;
        }
    },

    HasParameterIntent: (Language) => {
        switch(Language) {
            case RK.LANGUAGES.C:
                return false;
                break;
            case RK.LANGUAGES.CPP:
                return false;
                break;
            case RK.LANGUAGES.F90:
                return false;
                break;
            case RK.LANGUAGES.F08:
                return true;
                break;
            default:
                throw 'Language ' + Language + ' is not covered';
                break;
        }
    },

    ToOrdinal: (Number) => {
        var j = Number % 10,
            k = Number % 100;
        if (j == 1 && k != 11) {
            return Number + "st";
        }
        if (j == 2 && k != 12) {
            return Number + "nd";
        }
        if (j == 3 && k != 13) {
            return Number + "rd";
        }
        return Number + "th";
    },

    AssertInRange: (Entry, Property, PropertyValues) => {
        if(PropertyValues.includes(Entry[Property]) == false) {

            let ErrorMessage = Property + ' "' + Entry[Property] + '" not supported. Possible values are ';
            let FirstPropertyValue = true;
            PropertyValues.forEach((PropertyValue) => {
                if(FirstPropertyValue === false) {
                    ErrorMessage += ", ";
                }
                ErrorMessage += '"' + PropertyValue + '"';
                if(FirstPropertyValue === true) {
                    FirstPropertyValue = false;
                }
            });
            ErrorMessage += ".";
            throw ErrorMessage;
        }
    },

    AssertMustHave: (Entry, Property) => {
        if(Entry.hasOwnProperty(Property) == false) {
            throw 'Property ' + Property + ' missing.';
        }
    },

    AssertMustNotHave: (Entry, Property) => {
        if(Entry.hasOwnProperty(Property) == true) {
            throw 'Property ' + Property + ' was not supposed to be here.';
        }
    },

    AssertType: (Entry, Property, Expected) => {
        let Received = typeof Entry[Property];
        if(Array.isArray(Entry[Property]) == true) {
            Received = 'array';
        }
        if(Received !== Expected) {
            throw 'Property ' + Property + ' is of type ' + Received + ' instead of ' + Expected;
        }
    },

    AssertTypeSelf: (Entry, EntryLabel, Expected) => {
        let Received = typeof Entry;
        if(Array.isArray(Entry) == true) {
            Received = 'array';
        }
        if(Received !== Expected) {
            throw 'The ' + EntryLabel + ' is of type ' + Received + ' instead of ' + Expected;
        }
    },

    VerifyDocumentation: (Entry) => {
        // Check technology
        RK.AssertMustHave(Entry, RK.TechnologyPN);
        RK.AssertType(Entry, RK.TechnologyPN, "string");
        RK.AssertInRange(Entry, RK.TechnologyPN, Object.values(RK.TECHNOLOGIES));

        // Check name
        RK.AssertMustHave(Entry, RK.NamePN);
        RK.AssertType(Entry, RK.NamePN, "string");

        // Check directory name
        RK.AssertMustHave(Entry, RK.DirectoryNamePN);
        RK.AssertType(Entry, RK.DirectoryNamePN, "string");

        // Check shared description
        RK.AssertMustHave(Entry, RK.SharedDescriptionPN);
        RK.AssertType(Entry, RK.SharedDescriptionPN, "boolean");
        if(Entry[RK.SharedDescriptionPN] == true) {
            RK.AssertMustHave(Entry, RK.DescriptionPN);
            RK.AssertType(Entry, RK.DescriptionPN, "string");
        }

        // Check categories
        RK.AssertMustHave(Entry, RK.CategoriesPN);
        RK.AssertType(Entry, RK.CategoriesPN, "array");
        if(Entry[RK.CategoriesPN].length === 0) {
            throw 'No categories provided';
        }
        Entry[RK.CategoriesPN].forEach((Category) => {
            RK.AssertTypeSelf(Category, "category", "string");
        });

        // Check RK.LANGUAGES
        RK.AssertMustHave(Entry, RK.LanguagesPN);
        RK.AssertType(Entry, RK.LanguagesPN, "array");
        if(Entry[RK.LanguagesPN].length === 0) {
            throw RK.LanguagesPN + ' must have at least one entry';
        }
        RK.SetLocation(RK.LocationRoot + RK.LocationSeparator + RK.LanguagesPN);
        let LanguageCount = 1;
        Entry[RK.LanguagesPN].forEach((Language) => {
            RK.SetLocation(RK.LocationRoot + RK.LocationSeparator + RK.LanguagesPN + RK.LocationSeparator + RK.ToOrdinal(LanguageCount) + " " + RK.LanguagePN);
            RK.AssertTypeSelf(Language, RK.LanguagePN.toLowerCase() + " entry in " + RK.LanguagesPN.toLowerCase(), 'object');
            RK.AssertMustHave(Language, RK.LanguagePN);
            RK.AssertType(Language, RK.LanguagePN, 'string');
            RK.AssertInRange(Language, RK.LanguagePN, Object.values(RK.LANGUAGES));
            if(Entry[RK.SharedDescriptionPN] == false) {
                RK.AssertMustHave(Language, RK.DescriptionPN);
                RK.AssertType(Language, RK.DescriptionPN, "string");
            }
            else if(Language.hasOwnProperty(RK.DescriptionPN) == true) {
                throw 'Property "' + RK.SharedDescriptionPN + '" set to true, therefore description per language is not allowed.';
            }
            if(Language.hasOwnProperty(RK.ParametersPN) == true) {
                RK.AssertType(Language, RK.ParametersPN, "array");
                let ParameterCount = 1;
                RK.SetLocation(RK.LocationRoot + RK.LocationSeparator + RK.LanguagesPN + RK.LocationSeparator + RK.ToOrdinal(LanguageCount) + " " + RK.LanguagePN + RK.LocationSeparator + RK.ParametersPN);
                Language[RK.ParametersPN].forEach((Parameter) => {
                    RK.SetLocation(RK.LocationRoot + RK.LocationSeparator + RK.LanguagesPN + RK.LocationSeparator + RK.ToOrdinal(LanguageCount) + " " + RK.LanguagePN + RK.LocationSeparator + RK.ParametersPN + RK.LocationSeparator + RK.ToOrdinal(ParameterCount) + " " + RK.ParameterPN);
                    RK.AssertTypeSelf(Parameter, RK.ToOrdinal(ParameterCount) + " parameter entry in " + RK.ParametersPN, "object");
                    RK.AssertMustHave(Parameter, RK.NamePN);
                    RK.AssertType(Parameter, RK.NamePN, "string");
                    RK.AssertMustHave(Parameter, RK.DescriptionPN);
                    RK.AssertType(Parameter, RK.DescriptionPN, "string");
                    if(RK.IsParameterTypeRequired(Language[RK.LanguagePN]) == true) {
                        RK.AssertMustHave(Parameter, RK.TypePN);
                        RK.AssertType(Parameter, RK.TypePN, "string");
                    }
                    if(RK.CanParameterBeOptional(Language[RK.LanguagePN]) == true) {
                        RK.AssertMustHave(Parameter, RK.OptionalPN);
                        RK.AssertType(Parameter, RK.OptionalPN, "boolean");
                    }
                    if(RK.HasParameterIntent(Language[RK.LanguagePN]) == true) {
                        RK.AssertMustHave(Parameter, RK.IntentPN);
                        RK.AssertType(Parameter, RK.IntentPN, "string");
                        RK.AssertInRange(Parameter, RK.IntentPN, Object.values(RK.INTENTS));
                    }
                    if(Parameter.hasOwnProperty(RK.DimensionNumberPN) == true) {
                        RK.AssertType(Parameter, RK.DimensionNumberPN, "string");
                    }
                    ParameterCount++;
                });
                if(Language.hasOwnProperty(RK.ReturnPN)) {
                    RK.SetLocation(RK.LocationRoot + RK.LocationSeparator + RK.LanguagesPN + RK.LocationSeparator + RK.ToOrdinal(LanguageCount) + " " + RK.LanguagePN + RK.LocationSeparator + RK.ReturnPN);
                    RK.AssertType(Language, RK.ReturnPN, "object");
                    let Return = Language[RK.ReturnPN];
                    if(RK.IsParameterTypeRequired(Language[RK.LanguagePN])) {
                        RK.AssertMustHave(Return, RK.TypePN);
                        RK.AssertType(Return, RK.TypePN, "string")
                        RK.AssertMustHave(Return, RK.DescriptionPN);
                        RK.AssertType(Return, RK.DescriptionPN, "string")
                    }
                }
            }
            LanguageCount++;
        });
    },

    VerifyToolHomepage: (Entry) => {
        // Check technology
        RK.AssertMustHave(Entry, RK.TechnologyPN);
        RK.AssertType(Entry, RK.TechnologyPN, "string");
        RK.AssertInRange(Entry, RK.TechnologyPN, Object.values(RK.TECHNOLOGIES));

        // Check tools array
        RK.AssertMustHave(Entry, RK.ToolsPN);
        RK.AssertType(Entry, RK.ToolsPN, "array");

        Entry[RK.ToolsPN].forEach((Tool) => {
            RK.AssertTypeSelf(Tool, "tool", "object");
            
            // Check name
            RK.AssertMustHave(Tool, RK.NamePN);
            RK.AssertType(Tool, RK.NamePN, "string");
            
            // Check description
            RK.AssertMustHave(Tool, RK.DescriptionPN);
            RK.AssertType(Tool, RK.DescriptionPN, "string");
            
            // Check URL to tool itself
            RK.AssertMustHave(Tool, RK.UrlPN);
            RK.AssertType(Tool, RK.UrlPN, "string");
            
            // Check image URL
            RK.AssertMustHave(Tool, RK.ImageUrlPN);
            RK.AssertType(Tool, RK.ImageUrlPN, "string");
        });
    },

    VerifyExerciseHomepage: (Entry) => {
        // Check technology
        RK.AssertMustHave(Entry, RK.TechnologyPN);
        RK.AssertType(Entry, RK.TechnologyPN, "string");
        RK.AssertInRange(Entry, RK.TechnologyPN, Object.values(RK.TECHNOLOGIES));

        // Check exercises array
        RK.AssertMustHave(Entry, RK.ExercisesPN);
        RK.AssertType(Entry, RK.ExercisesPN, "array");

        Entry[RK.ExercisesPN].forEach((Exercise) => {
            RK.AssertTypeSelf(Exercise, "exercise", "object");
            
            // Check name
            RK.AssertMustHave(Exercise, RK.NamePN);
            RK.AssertType(Exercise, RK.NamePN, "string");
            
            // Check description
            RK.AssertMustHave(Exercise, RK.DescriptionPN);
            RK.AssertType(Exercise, RK.DescriptionPN, "string");
            
            // Check difficulty
            RK.AssertMustHave(Exercise, RK.DifficultyPN);
            RK.AssertType(Exercise, RK.DifficultyPN, "number");
        });
    },

    VerifyExercise: (Entry) => {
        // Check technology
        RK.AssertMustHave(Entry, RK.TechnologyPN);
        RK.AssertType(Entry, RK.TechnologyPN, "string");
        RK.AssertInRange(Entry, RK.TechnologyPN, Object.values(RK.TECHNOLOGIES));

        // Check title
        RK.AssertMustHave(Entry, RK.TitlePN);
        RK.AssertType(Entry, RK.TitlePN, "string");
        
        // Check introduction
        RK.AssertMustHave(Entry, RK.IntroductionPN);
        RK.AssertType(Entry, RK.IntroductionPN, "string");
        
        // Check difficulty
        RK.AssertMustHave(Entry, RK.DifficultyPN);
        RK.AssertType(Entry, RK.DifficultyPN, "number");
        
        // Check hints
        if(Entry.hasOwnProperty(RK.HintsPN)) {
            RK.AssertType(Entry, RK.HintsPN, "string");
        }
    },

    VerifyDocumentationHomepage: (Entry) => {
        // Check technology
        RK.AssertMustHave(Entry, RK.TechnologyPN);
        RK.AssertType(Entry, RK.TechnologyPN, "string");
        RK.AssertInRange(Entry, RK.TechnologyPN, Object.values(RK.TECHNOLOGIES));
    },

    VerifyAbout: (Entry) => {
        // Check technology
        RK.AssertMustHave(Entry, RK.TechnologyPN);
        RK.AssertType(Entry, RK.TechnologyPN, "string");
        RK.AssertInRange(Entry, RK.TechnologyPN, Object.values(RK.TECHNOLOGIES));

        RK.AssertMustHave(Entry, RK.SectionsPN);
        RK.AssertType(Entry, RK.SectionsPN, "array");
        Entry[RK.SectionsPN].forEach((Section) => {
            RK.AssertTypeSelf(Section, "section", "object");

            // Check title is here
            RK.AssertMustHave(Section, RK.TitlePN);
            RK.AssertType(Section, RK.TitlePN, "string");

            // Check content is here
            RK.AssertMustHave(Section, RK.ContentPN);
            RK.AssertType(Section, RK.ContentPN, "string");
        });
    },

    VerifyHomepage: (Entry) => {
        // Check technologies
        RK.AssertMustHave(Entry, RK.TechnologiesPN);
        RK.AssertType(Entry, RK.TechnologiesPN, "array");
    
        Entry[RK.TechnologiesPN].forEach((Technology) => {
            // Check that the array entry is an object
            RK.AssertTypeSelf(Technology, "technology", "object");

            // Check technology
            RK.AssertMustHave(Technology, RK.TechnologyPN);
            RK.AssertType(Technology, RK.TechnologyPN, "string");
            RK.AssertInRange(Technology, RK.TechnologyPN, Object.values(RK.TECHNOLOGIES));

            // Check technology
            RK.AssertMustHave(Technology, RK.DescriptionPN);
            RK.AssertType(Technology, RK.DescriptionPN, "string");
        });
    },

    VerifyText: (Entry) => {
        // Check title
        RK.AssertMustHave(Entry, RK.TitlePN);
        RK.AssertType(Entry, RK.TitlePN, "string");

        // Check sections
        RK.AssertMustHave(Entry, RK.SectionsPN);
        RK.AssertType(Entry, RK.SectionsPN, "array");

        Entry[RK.SectionsPN].forEach((Section) => {
            // Check that the array entry is an object
            RK.AssertTypeSelf(Section, "section", "object");

            // Check title
            RK.AssertMustHave(Section, RK.TitlePN);
            RK.AssertType(Section, RK.TitlePN, "string");

            // Check content
            RK.AssertMustHave(Section, RK.ContentPN);
            RK.AssertType(Section, RK.ContentPN, "string");
        });
    },

    /**
     * @brief Will check that the Entry is a valid one from a semantic point of view.
     * @param[in] Entry Th,e entry, as a valid JSON object.
     **/
    Verify: (Entry) => {
        try{
            if(Entry == null) {
                throw 'Null entry.';
            }
            if(Entry === "") {
                throw 'Empty entry.';
            }
            RK.AssertTypeSelf(Entry, "entry", "object");
            RK.AssertMustHave(Entry, RK.TypePN);
            RK.AssertType(Entry, RK.TypePN, "string");
            RK.AssertInRange(Entry, RK.TypePN, Object.values(RK.ENTRY_TYPES));
            switch(Entry.Type) {
                case RK.ENTRY_TYPES.TEXT:
                    RK.VerifyText(Entry);
                    break;
                case RK.ENTRY_TYPES.ABOUT:
                    RK.VerifyAbout(Entry);
                    break;
                case RK.ENTRY_TYPES.DOCUMENTATION:
                    RK.VerifyDocumentation(Entry);
                    break;
                case RK.ENTRY_TYPES.TOOL:
                    throw "Tool entry found; however it has not been implemented just yet."
                    break;
                case RK.ENTRY_TYPES.TOOL_HOMEPAGE:
                    RK.VerifyToolHomepage(Entry);
                    break;
                case RK.ENTRY_TYPES.EXERCISE_HOMEPAGE:
                    RK.VerifyExerciseHomepage(Entry);
                    break;
                case RK.ENTRY_TYPES.EXERCISE:
                    RK.VerifyExercise(Entry);
                    break;
                case RK.ENTRY_TYPES.DOCUMENTATION_HOMEPAGE:
                    RK.VerifyDocumentationHomepage(Entry);
                    break;
                case RK.ENTRY_TYPES.HOMEPAGE:
                    RK.VerifyHomepage(Entry);
                    break;
                default:
                    throw 'Unknown entry type.';
                    break;
            }
        }
        catch(e) {
            throw RK.LocationOpener + "Error at " + RK.GetLocation() + RK.LocationCloser + e;
        }
    },

    /**************
     * GENERATION *
     **************/
    ShowVersion: (Event, Language) => {
        const Versions = document.getElementsByClassName('Version');
        [].forEach.call(Versions, (Version) => {
            Version.style.display = "none";
        });
        
        [].forEach.call(document.getElementsByClassName(Language), (L) => {
            L.style.display = "";
        });
    
        const VersionTabs = document.getElementsByClassName('VersionTab');
        [].forEach.call(VersionTabs, (VersionTab) => {
            VersionTab.classList.remove("Active");
        });
        [].forEach.call(VersionTabs, (VersionTab) => {
            if(VersionTab.innerText === Language) {
                VersionTab.classList.add("Active");
            }
        });
    },

    FirstExampleFetched: (XHR, LanguageDocs, LanguageClass, LanguageFileFormat) => {
        switch(XHR.status) {
            case 0:
            case 200:
                // There is at least one example
                // Create the example page
                const ExamplePage = document.createElement('article');
                ExamplePage.classList.add("PageArticle")
                LanguageDocs.appendChild(ExamplePage);
        
                const ExamplePageHeader = document.createElement('div');
                ExamplePageHeader.classList.add("PageArticleHeader")
                ExamplePage.appendChild(ExamplePageHeader);
        
                const ExamplePageHeaderTitle = document.createElement('h2');
                ExamplePageHeaderTitle.innerText = 'Example';
                ExamplePageHeader.appendChild(ExamplePageHeaderTitle);
        
                const ExamplePageBody = document.createElement('div');
                ExamplePageBody.classList.add("PageArticleBody")
                ExamplePage.appendChild(ExamplePageBody);

                RK.CreateCode(XHR.responseText, ExamplePageBody, LanguageClass);
                const NewXHR = new XMLHttpRequest();
                const PathToExample = RK.FolderPath + "/example_2" + "." + LanguageFileFormat;
                NewXHR.open("GET", PathToExample, true);
                NewXHR.onload = () => {
                    RK.SecondExampleAndBeyondFetched(NewXHR, LanguageDocs, LanguageClass, ExamplePageBody, LanguageFileFormat, 2);
                };
                NewXHR.send(null);
                break;
            case 404:
                // No example is provided so far.
                break;
            default:
                alert("Not expected status code: " + XHR.status);
                break;
        }
    },

    SecondExampleAndBeyondFetched: (XHR, LanguageDocs, LanguageClass, ExamplePageBody, LanguageFileFormat, ExampleCounter) => {
        switch(XHR.status) {
            case 0:
            case 200:
                // Add the example to the already generated page
                RK.CreateCode(XHR.responseText, ExamplePageBody, LanguageClass);
                const NewXHR = new XMLHttpRequest();
                const PathToExample = RK.FolderPath + "/example_" + parseInt(ExampleCounter + 1) + "." + LanguageFileFormat;
                NewXHR.open("GET", PathToExample, true);
                NewXHR.onload = () => {
                    RK.SecondExampleAndBeyondFetched(NewXHR, LanguageDocs, LanguageClass, ExamplePageBody, LanguageFileFormat, ExampleCounter+1);
                };
                NewXHR.send(null);
                break;
            case 404:
                // No example is provided so far.
                break;
            default:
                alert("Not expected status code: " + XHR.status);
                break;
        }
    },

    InsertCrossReferencesFromTechnology: (Text) => {
        const TechnologyName = window.location.pathname.split("/")[1];
        let Technology = null;
        for(let I = 0; I < window.Manifest.length; I++) {
            if(Manifest[I][RK.TechnologyPN].toLowerCase() === TechnologyName) {
                Technology = window.Manifest[I];
            }
        }
        if(Technology == null) {
            return Text;
        }
        else {
            const Entries = Technology[RK.EntriesPN];
            return Text.replace(/\b\w+\b/g, (Word) => {
                for(let I = 0; I < Entries.length; I++) {
                    const Entry = Entries[I];
                    if(Entry[RK.NamePN] === Word) {
                        return '<a href="' + Technology['PathRoot'] + '/' + Entry[RK.DirectoryNamePN] + '/index.html">' + Word + '</a>';
                    }
                }
                return Word;
            });
        }
    },

    InterpretMarkdown: (Text) => {
        let TextGenerated = "";
        let AllLines = Text.split(/\r?\n/)
        let InListMode = false;
        let FirstLine = true;
        let FirstLineAfterListModeEnded = false;
        for(let I = 0; I < AllLines.length; I++)
        {
            if(FirstLine === true)
            {
                FirstLine = false;
            }
            else if(InListMode === false)
            {
                if(FirstLineAfterListModeEnded === true)
                {
                    FirstLineAfterListModeEnded = false;
                }
                else
                {
                    TextGenerated += "<br>";
                }
            }
            if(AllLines[I].startsWith('-'))
            {
                if(InListMode === false)
                {
                    InListMode = true;
                    TextGenerated += "<ul>";
                }
                TextGenerated += "<li>" + AllLines[I].substring(1) + "</li>";
            }
            else
            {
                if(InListMode === true)
                {
                    TextGenerated += "</ul>";
                    InListMode = false;
                    FirstLineAfterListModeEnded = true;
                }
                TextGenerated += AllLines[I];
            }
        }
        if(InListMode === true)
        {
            TextGenerated += "</ul>";
            InListMode = false;
        }
        return TextGenerated;
    },
    
    GenerateDocumentation: async (Entry) => {
        // Build the technology header containing the about / docs / tools / exercises tabs
        RK.BuildTechnologyNav(Entry);

        // Build header
        const MainSection = document.createElement('section');
        MainSection.id = "MainSection";
        RK.GetOutput().appendChild(MainSection);
    
        const MainSectionDiv = document.createElement('div');
        MainSection.appendChild(MainSectionDiv);
    
        // Build category group
        const CategoryGroup = document.createElement('div');
        CategoryGroup.classList.add('DocumentationCategoryGroup');
        MainSectionDiv.appendChild(CategoryGroup);
    
        // Add categories
        Entry[RK.CategoriesPN].forEach((Category) => {
            const CategoryDiv = document.createElement('div');
            CategoryDiv.classList.add("DocumentationEntryCategory");
            CategoryDiv.classList.add("FakeButton");
            CategoryDiv.onclick = () => {
                window.location.href = RK.BASE_URL + '/' + Entry[RK.TechnologyPN].toLowerCase() + '/docs/index.html#' + Category;
            };
            CategoryGroup.appendChild(CategoryDiv);
    
            const SvgImgPath = "url('" + RK.BASE_URL + "/images/" + Entry[RK.TechnologyPN].toLowerCase() + "/" + Category.replace(' ', '_') + ".svg')";
            const CategoryImage = document.createElement('div');
            CategoryImage.classList.add("DocumentationEntryCategoryImage");
            CategoryImage.setAttribute('mask-image', SvgImgPath);
            CategoryImage.style.webkitMaskImage = SvgImgPath;
            CategoryDiv.appendChild(CategoryImage);
    
            const CategoryText = document.createElement('p');
            CategoryText.innerText = Category;
            CategoryDiv.appendChild(CategoryText);
        });
    
        // Build div for language tabs
        const LanguageTabs = document.createElement('div');
        LanguageTabs.id = "VersionTabs";
        MainSectionDiv.appendChild(LanguageTabs);

        const LanguageTabsParagraph = document.createElement('p');
        LanguageTabs.appendChild(LanguageTabsParagraph);
    
        const MainSectionDivHeader = document.createElement('header');
        MainSectionDiv.appendChild(MainSectionDivHeader);
    
        const Header = document.createElement('h1');
        Header.innerHTML = Entry[RK.NamePN];
        MainSectionDivHeader.appendChild(Header);
    
        Entry[RK.LanguagesPN].forEach((LanguageEntry, Index) => {
            // Create the language tab
            let LanguageTab = document.createElement('span');
            LanguageTab.classList.add("VersionTab");
            LanguageTab.classList.add("FakeButton");
            LanguageTab.innerText = LanguageEntry[RK.LanguagePN];
            if(Index === 0) {
                LanguageTab.classList.add("active");
            }
            if(Index > 0) {
                LanguageTabsParagraph.innerHTML += " | ";
            }
            LanguageTabsParagraph.appendChild(LanguageTab);
    
            // Create the language documentation
            const LanguageDocs = document.createElement('div');
            LanguageDocs.classList.add("Version", LanguageEntry[RK.LanguagePN]);
            MainSection.appendChild(LanguageDocs);
            if(Index === 0) {
                LanguageDocs.style.display = "block";
            }
            else {
                LanguageDocs.style.display = "none";
            }
    
            // Create the definition page
            const DefinitionPage = document.createElement('article');
            DefinitionPage.classList.add("PageArticle")
            LanguageDocs.appendChild(DefinitionPage);
    
            const DefinitionPageHeader = document.createElement('div');
            DefinitionPageHeader.classList.add("PageArticleHeader")
            DefinitionPage.appendChild(DefinitionPageHeader);
    
            const DefinitionPageHeaderTitle = document.createElement('h2');
            DefinitionPageHeaderTitle.innerText = 'Definition';
            DefinitionPageHeader.appendChild(DefinitionPageHeaderTitle);
    
            const DefinitionPageBody = document.createElement('div');
            DefinitionPageBody.classList.add("PageArticleBody")
            DefinitionPage.appendChild(DefinitionPageBody);
    
            const DefinitionPageBodyContent = document.createElement('p');
            if(Entry[RK.SharedDescriptionPN] == false) {
                DefinitionPageBodyContent.innerHTML = RK.InterpretMarkdown(RK.InsertCrossReferencesFromTechnology(LanguageEntry[RK.DescriptionPN]));
            }
            else {
                DefinitionPageBodyContent.innerHTML = RK.InterpretMarkdown(RK.InsertCrossReferencesFromTechnology(Entry[RK.DescriptionPN]));
            }
            DefinitionPageBody.appendChild(DefinitionPageBodyContent);
    
            let ParametersPage = null;

            let LanguageClass;
            let LanguageFileFormat;
            switch(LanguageEntry[RK.LanguagePN]) {
                case RK.LANGUAGES.C:
                    LanguageClass = RK.LANGUAGE_HLJS_CLASS.C;
                    LanguageFileFormat = RK.LANGUAGE_FILE_EXTENSIONS.C;
                    if(LanguageEntry.hasOwnProperty(RK.ParametersPN) == true) {
                        let PrototypeString = "";
                        PrototypeString += (LanguageEntry.hasOwnProperty(RK.ReturnPN) ? LanguageEntry[RK.ReturnPN][RK.TypePN]  : "void") + " " + Entry[RK.NamePN] + "(";
                        const PrototypeStringLength = PrototypeString.length;
    
                        let DefinitionList = null;
                        let FirstParameter = true;
                        if(LanguageEntry[RK.ParametersPN].length === 0) {
                            PrototypeString += "void";
                        }
                        else {
                            // Fill the parameters page
                            ParametersPage = document.createElement('article');
                            ParametersPage.classList.add("PageArticle");
                            // Do not add it straight away to the main section, we first need to add the prototype after we parse parameters
    
                            const ParametersPageHeader = document.createElement('div');
                            ParametersPageHeader.classList.add("PageArticleHeader")
                            ParametersPage.appendChild(ParametersPageHeader);
    
                            const ParametersPageHeaderTitle = document.createElement('h2');
                            ParametersPageHeaderTitle.innerText = RK.ParametersPN;
                            ParametersPageHeader.appendChild(ParametersPageHeaderTitle);
    
                            const ParametersPageBody = document.createElement('div');
                            ParametersPageBody.classList.add("PageArticleBody")
                            ParametersPage.appendChild(ParametersPageBody);
                            
                            LanguageEntry[RK.ParametersPN].forEach((ParameterEntry) => {
                                if(FirstParameter == true) {
                                    DefinitionList = document.createElement('dl');
                                }
                                else {
                                    PrototypeString += ",\n";
                                    for(var i = 0; i < PrototypeStringLength; i++) {
                                        PrototypeString += " ";
                                    }
                                }
                                PrototypeString += ParameterEntry[RK.TypePN].split(RK.ParameterTypeSplitter)[0] + " " + ParameterEntry[RK.NamePN] + (ParameterEntry[RK.TypePN].split(RK.ParameterTypeSplitter)[1] || '');
                                if(ParameterEntry.hasOwnProperty(RK.DimensionNumberPN) == true) {
                                    PrototypeString += "[" + ParameterEntry[RK.DimensionNumberPN] + "]";
                                }
                                let DefinitionTerm = document.createElement('dt');
                                DefinitionTerm.innerText = ParameterEntry[RK.NamePN];
                                if(ParameterEntry.hasOwnProperty(RK.OptionalPN) == true) {
                                    if(ParameterEntry[RK.OptionalPN] === true) {
                                        DefinitionTerm.innerHTML += " <span class='OptionalFlag'>[Optional]</span>";
                                    }
                                }
                                DefinitionList.appendChild(DefinitionTerm);
                                let DefinitionDescription = document.createElement('dd');
                                DefinitionDescription.innerHTML = RK.InterpretMarkdown(RK.InsertCrossReferencesFromTechnology(ParameterEntry[RK.DescriptionPN]));
                                DefinitionList.appendChild(DefinitionDescription);
                                if(FirstParameter == true) {
                                    FirstParameter = false;
                                }
                            });
    
                            ParametersPageBody.appendChild(DefinitionList);
                        }
    
                        PrototypeString += ");";
                        if(LanguageEntry.hasOwnProperty(RK.PrototypePN) == true)
                        {
                            PrototypeString = LanguageEntry[RK.PrototypePN];
                        }
                        RK.CreateCode(PrototypeString, DefinitionPageBody, RK.LANGUAGE_HLJS_CLASS.C);
    
                        // If there was at least one parameter, add the parameters section
                        if(ParametersPage != null) {
                            LanguageDocs.appendChild(ParametersPage);
                        }
    
                        // Create the return part if any
                        if(LanguageEntry.hasOwnProperty(RK.ReturnPN))
                        {
                            const ReturnPage = document.createElement('article');
                            ReturnPage.classList.add("PageArticle")
                            LanguageDocs.appendChild(ReturnPage);
        
                            const ReturnPageHeader = document.createElement('div');
                            ReturnPageHeader.classList.add("PageArticleHeader")
                            ReturnPage.appendChild(ReturnPageHeader);
        
                            const ReturnPageHeaderTitle = document.createElement('h2');
                            ReturnPageHeaderTitle.innerText = RK.ReturnPN;
                            ReturnPageHeader.appendChild(ReturnPageHeaderTitle);
        
                            const ParametersPageBody = document.createElement('div');
                            ParametersPageBody.classList.add("PageArticleBody")
                            ReturnPage.appendChild(ParametersPageBody);
        
                            let ReturnDescription = document.createElement('p');
                            ReturnDescription.innerHTML = RK.InterpretMarkdown(RK.InsertCrossReferencesFromTechnology(LanguageEntry[RK.ReturnPN][RK.DescriptionPN]));
                            ParametersPageBody.appendChild(ReturnDescription);
                        }
                    }
                    break;
                case RK.LANGUAGES.F90:
                    LanguageClass = RK.LANGUAGE_HLJS_CLASS.F90;
                    LanguageFileFormat = RK.LANGUAGE_FILE_EXTENSIONS.F90;
                    if(LanguageEntry.hasOwnProperty(RK.ParametersPN) == true) {
                        let PrototypeString = "";
                        if(LanguageEntry.hasOwnProperty(RK.ReturnPN) == true) {
                            PrototypeString += "FUNCTION";
                        } else {
                            PrototypeString += "PROCEDURE";
                        }
                        PrototypeString += " " + Entry[RK.NamePN] + "(";
                        const PrototypeStringLength = PrototypeString.length;
    
                        let DefinitionList = null;
                        let FirstParameter = true;
                        if(LanguageEntry[RK.ParametersPN].length === 0) {
                            PrototypeString += "";
                        }
                        else {
                            // Fill the parameters page
                            ParametersPage = document.createElement('article');
                            ParametersPage.classList.add("PageArticle");
                            // Do not add it straight away to the main section, we first need to add the prototype after we parse parameters
    
                            const ParametersPageHeader = document.createElement('div');
                            ParametersPageHeader.classList.add("PageArticleHeader")
                            ParametersPage.appendChild(ParametersPageHeader);
    
                            const ParametersPageHeaderTitle = document.createElement('h2');
                            ParametersPageHeaderTitle.innerText = RK.ParametersPN;
                            ParametersPageHeader.appendChild(ParametersPageHeaderTitle);
    
                            const ParametersPageBody = document.createElement('div');
                            ParametersPageBody.classList.add("PageArticleBody")
                            ParametersPage.appendChild(ParametersPageBody);
                            
                            LanguageEntry[RK.ParametersPN].forEach((ParameterEntry) => {
                                if(FirstParameter == true) {
                                    DefinitionList = document.createElement('dl');
                                }
                                else {
                                    PrototypeString += ", ";
                                }
                                PrototypeString += ParameterEntry[RK.NamePN];
                                let DefinitionTerm = document.createElement('dt');
                                DefinitionTerm.innerText = ParameterEntry[RK.NamePN];
                                if(ParameterEntry.hasOwnProperty(RK.OptionalPN) == true) {
                                    if(ParameterEntry[RK.OptionalPN] === true) {
                                        DefinitionTerm.innerHTML += " <span class='OptionalFlag'>[Optional]</span>";
                                    }
                                }
                                DefinitionList.appendChild(DefinitionTerm);
                                let DefinitionDescription = document.createElement('dd');
                                DefinitionDescription.innerHTML = RK.InterpretMarkdown(RK.InsertCrossReferencesFromTechnology(ParameterEntry[RK.DescriptionPN]));
                                DefinitionList.appendChild(DefinitionDescription);
                                if(FirstParameter == true) {
                                    FirstParameter = false;
                                }
                            });
    
                            ParametersPageBody.appendChild(DefinitionList);
                        }
    
                        PrototypeString += ")";
                        LanguageEntry[RK.ParametersPN].forEach((ParameterEntry) => {
                            PrototypeString += "\n    " + ParameterEntry[RK.TypePN].split(RK.ParameterTypeSplitter)[0] + " :: " + ParameterEntry[RK.NamePN] + (ParameterEntry[RK.TypePN].split(RK.ParameterTypeSplitter)[1] || '');
                            if(ParameterEntry.hasOwnProperty(RK.DimensionNumberPN) == true) {
                                PrototypeString += "(" + ParameterEntry[RK.DimensionNumberPN] + ")";
                            }
                        });

                        if(LanguageEntry.hasOwnProperty(RK.ReturnPN) == true) {
                            PrototypeString += "\n    " + LanguageEntry[RK.ReturnPN][RK.TypePN] + " :: " + Entry[RK.NamePN];
                        }
    
                        if(LanguageEntry.hasOwnProperty(RK.PrototypePN) == true)
                        {
                            PrototypeString = LanguageEntry[RK.PrototypePN];
                        }
                        RK.CreateCode(PrototypeString, DefinitionPageBody, RK.LANGUAGE_HLJS_CLASS.F90);
    
                        // If there was at least one parameter, add the parameters section
                        if(ParametersPage != null) {
                            LanguageDocs.appendChild(ParametersPage);
                        }
    
                        // Create the return part if any
                        if(LanguageEntry.hasOwnProperty(RK.ReturnPN) == true) {
                            const ReturnPage = document.createElement('article');
                            ReturnPage.classList.add("PageArticle")
                            LanguageDocs.appendChild(ReturnPage);
    
                            const ReturnPageHeader = document.createElement('div');
                            ReturnPageHeader.classList.add("PageArticleHeader")
                            ReturnPage.appendChild(ReturnPageHeader);
    
                            const ReturnPageHeaderTitle = document.createElement('h2');
                            ReturnPageHeaderTitle.innerText = RK.ReturnPN;
                            ReturnPageHeader.appendChild(ReturnPageHeaderTitle);
    
                            const ParametersPageBody = document.createElement('div');
                            ParametersPageBody.classList.add("PageArticleBody")
                            ReturnPage.appendChild(ParametersPageBody);
    
                            let ReturnDescription = document.createElement('p');
                            ReturnDescription.innerHTML = RK.InterpretMarkdown(RK.InsertCrossReferencesFromTechnology(LanguageEntry[RK.ReturnPN][RK.DescriptionPN]));
                            ParametersPageBody.appendChild(ReturnDescription);
                        }
                    }
                    break;
                case RK.LANGUAGES.F08:
                    LanguageClass = RK.LANGUAGE_HLJS_CLASS.F08;
                    LanguageFileFormat = RK.LANGUAGE_FILE_EXTENSIONS.F08;
                    if(LanguageEntry.hasOwnProperty(RK.ParametersPN) == true) {
                        let PrototypeString = "";
                        if(LanguageEntry.hasOwnProperty(RK.ReturnPN) == true) {
                            PrototypeString += "FUNCTION";
                        } else {
                            PrototypeString += "PROCEDURE";
                        }
                        PrototypeString += " " + Entry[RK.NamePN] + "(";
                        const PrototypeStringLength = PrototypeString.length;
    
                        let DefinitionList = null;
                        let FirstParameter = true;
                        if(LanguageEntry[RK.ParametersPN].length > 0) {
                            // Fill the parameters page
                            ParametersPage = document.createElement('article');
                            ParametersPage.classList.add("PageArticle");
                            // Do not add it straight away to the main section, we first need to add the prototype after we parse parameters
    
                            const ParametersPageHeader = document.createElement('div');
                            ParametersPageHeader.classList.add("PageArticleHeader")
                            ParametersPage.appendChild(ParametersPageHeader);
    
                            const ParametersPageHeaderTitle = document.createElement('h2');
                            ParametersPageHeaderTitle.innerText = RK.ParametersPN;
                            ParametersPageHeader.appendChild(ParametersPageHeaderTitle);
    
                            const ParametersPageBody = document.createElement('div');
                            ParametersPageBody.classList.add("PageArticleBody")
                            ParametersPage.appendChild(ParametersPageBody);
                            
                            LanguageEntry[RK.ParametersPN].forEach((ParameterEntry) => {
                                if(FirstParameter == true) {
                                    DefinitionList = document.createElement('dl');
                                }
                                else {
                                    PrototypeString += ", ";
                                }
                                PrototypeString += ParameterEntry[RK.NamePN];
                                let DefinitionTerm = document.createElement('dt');
                                DefinitionTerm.innerText = ParameterEntry[RK.NamePN];
                                if(ParameterEntry.hasOwnProperty(RK.OptionalPN) == true) {
                                    if(ParameterEntry[RK.OptionalPN] === true) {
                                        DefinitionTerm.innerHTML += " <span class='OptionalFlag'>[Optional]</span>";
                                    }
                                }
                                DefinitionList.appendChild(DefinitionTerm);
                                let DefinitionDescription = document.createElement('dd');
                                DefinitionDescription.innerHTML = RK.InterpretMarkdown(RK.InsertCrossReferencesFromTechnology(ParameterEntry[RK.DescriptionPN]));
                                DefinitionList.appendChild(DefinitionDescription);
                                if(FirstParameter == true) {
                                    FirstParameter = false;
                                }
                            });
    
                            ParametersPageBody.appendChild(DefinitionList);
                        }
    
                        PrototypeString += ")";
                        let HasAtLeastOneCPtrArgument = false;
                        LanguageEntry[RK.ParametersPN].forEach((ParameterEntry) => {
                            if(ParameterEntry[RK.TypePN].includes("TYPE(C_PTR)")) {
                                HasAtLeastOneCPtrArgument = true;
                            }
                        });
                        if(HasAtLeastOneCPtrArgument) {
                            PrototypeString += "\n    USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_PTR";
                        }
                        LanguageEntry[RK.ParametersPN].forEach((ParameterEntry) => {
                            PrototypeString += "\n    " + ParameterEntry[RK.TypePN].split(RK.ParameterTypeSplitter)[0];
                            if(RK.CanParameterBeOptional(LanguageEntry[RK.LanguagePN]) == true) {
                                if(ParameterEntry[RK.OptionalPN] === true) {
                                    PrototypeString += ", OPTIONAL";
                                }
                            }
                            PrototypeString += ", INTENT(" + ParameterEntry[RK.IntentPN] + ") :: " + ParameterEntry[RK.NamePN] + (ParameterEntry[RK.TypePN].split(RK.ParameterTypeSplitter)[1] || '');
                            if(ParameterEntry.hasOwnProperty(RK.DimensionNumberPN) == true) {
                                PrototypeString += "(" + ParameterEntry[RK.DimensionNumberPN] + ")";
                            }
                        });

                        if(LanguageEntry.hasOwnProperty(RK.ReturnPN) == true) {
                            PrototypeString += "\n    " + LanguageEntry[RK.ReturnPN][RK.TypePN] + " :: " + Entry[RK.NamePN];
                        }
    
                        if(LanguageEntry.hasOwnProperty(RK.PrototypePN) == true)
                        {
                            PrototypeString = LanguageEntry[RK.PrototypePN];
                        }
                        RK.CreateCode(PrototypeString, DefinitionPageBody, RK.LANGUAGE_HLJS_CLASS.F08);
    
                        // If there was at least one parameter, add the parameters section
                        if(ParametersPage != null) {
                            LanguageDocs.appendChild(ParametersPage);
                        }
    
                        // Create the return part if any
                        if(LanguageEntry.hasOwnProperty(RK.ReturnPN) == true) {
                            const ReturnPage = document.createElement('article');
                            ReturnPage.classList.add("PageArticle")
                            LanguageDocs.appendChild(ReturnPage);
    
                            const ReturnPageHeader = document.createElement('div');
                            ReturnPageHeader.classList.add("PageArticleHeader")
                            ReturnPage.appendChild(ReturnPageHeader);
    
                            const ReturnPageHeaderTitle = document.createElement('h2');
                            ReturnPageHeaderTitle.innerText = RK.ReturnPN;
                            ReturnPageHeader.appendChild(ReturnPageHeaderTitle);
    
                            const ParametersPageBody = document.createElement('div');
                            ParametersPageBody.classList.add("PageArticleBody")
                            ReturnPage.appendChild(ParametersPageBody);
    
                            let ReturnDescription = document.createElement('p');
                            ReturnDescription.innerHTML = RK.InterpretMarkdown(RK.InsertCrossReferencesFromTechnology(LanguageEntry[RK.ReturnPN][RK.DescriptionPN]));
                            ParametersPageBody.appendChild(ReturnDescription);
                        }
                    }
                    break;
                default:
                    throw 'Language entry not known.';
                    break;
            }

            if(!RK.IsEditorMode()) {
                // Fetch examples
                let ExampleCounter = 1;
                const XHR = new XMLHttpRequest();
                const PathToExample = RK.FolderPath + "/example_" + ExampleCounter + "." + LanguageFileFormat;
                XHR.open("GET", PathToExample, true);
                XHR.onload = () => {
                    RK.FirstExampleFetched(XHR, LanguageDocs, LanguageClass, LanguageFileFormat);
                };
                XHR.send(null);
            }
        });
    
        [].forEach.call(document.getElementsByClassName('VersionTab'), (VersionTab) => {
            VersionTab.onclick = () => {
                RK.ShowVersion(Event, VersionTab.innerText);
            };
        });
        document.getElementsByClassName('VersionTab')[0].click();
    },

    GenerateDocumentationHomepage: async (Entry) => {
        RK.BuildTechnologyNav(Entry);
        var IndexHTML = '<section id="MainSection"><div><header><h1>' + Entry[RK.TechnologyPN] + ' Documentation</h1></header></div><div style="display:flex; flex-wrap: wrap; align-items: stretch; column-gap:20px;">';
        Object.keys(Entry[RK.CategoriesPN]).forEach((Category) => {
            IndexHTML += `
                <article class="DocumentationCategory" id="` + Category + `">
                    <div class="DocumentationCategoryHeader">
                        <div class="DocumentationCategoryLogo" style="mask-image: url('` + RK.BASE_URL + `/images/` + Entry[RK.TechnologyPN].toLowerCase() + `/` + Category.replace(' ', '_') + `.svg'); -webkit-mask-image: url('` + RK.BASE_URL + `/images/` + Entry[RK.TechnologyPN].toLowerCase() + `/` + Category.replace(' ', '_') + `.svg');"></div>
                        <h1>` + Category + `</h1>
                    </div>
                    <div class="DocumentationCategoryBody">
                        <ul>`;
            Entry[RK.CategoriesPN][Category].forEach((CategorisedEntry) => {
                IndexHTML += `<li><a href = "` + RK.BASE_URL + `/` + Entry[RK.TechnologyPN].toLowerCase() + `/docs/` + CategorisedEntry.toLowerCase() + `/index.html">` + CategorisedEntry.replace('_', '_<wbr>') + `</a></li>`;
            });
            IndexHTML += `
                        </ul>
                    </div>
                </article>`;
        });
        IndexHTML += '</div></section>';
        RK.GetOutput().innerHTML += IndexHTML;
    },

    GenerateHomepage: async (Entry) => {
        let InnerHTML = `
        <div id="SubHeader">
			<p>You are looking to learn about High-Performance Computing? <a href = "` + RK.BASE_URL + `/">RookieHPC</a> is a great place to be! If you have any question, please contact us either using the <a href = "mailto:` + RK.CONTACT_EMAIL + `">contact form</a>, or follow us on <a href = "https://twitter.com/RookieHPC">Twitter</a>.</p>
		</div>
        <section id="MainSection">
            <h1>Pick your technology</h1>
                <div id="TechnologyArticles">`;
        const XHR = new XMLHttpRequest();
        XHR.open('GET', RK.BASE_URL + '/data.json', false);
        XHR.onload = () => {
            switch(XHR.status) {
                case 0:
                case 200:
                    JsonData = JSON.parse(XHR.responseText);
                    JsonData[RK.TechnologiesPN].forEach((Technology) => {
                        InnerHTML += `
                        <article class="TechnologyArticle">
                            <header>
                                <div class="Tab DocsTab" onclick="location.href='` + RK.BASE_URL + `/` + Technology[RK.TechnologyPN].toLowerCase() + `/docs/index.html'">
                                    <div class="Logo"></div>
                                    <p class="Label">Docs</p>
                                </div>
                                <div class="Tab ToolsTab" onclick="location.href='` + RK.BASE_URL + `/` + Technology[RK.TechnologyPN].toLowerCase() + `/tools/index.html'">
                                    <div class="Logo"></div>
                                    <p class="Label">Tools</p>
                                </div>
                                <div class="Tab ExercisesTab" onclick="location.href='` + RK.BASE_URL + `/` + Technology[RK.TechnologyPN].toLowerCase() + `/exercises/index.html'">
                                    <div class="Logo"></div>
                                    <p class="Label">Exercises</p>
                                </div>
                            </header>
                            <div class="TechnologyCard">
                                <div class="ArticleType" style="mask-image: url('` + RK.BASE_URL + `/images/` + Technology[RK.TechnologyPN].toLowerCase() + `.svg'); -webkit-mask-image: url('` + RK.BASE_URL + `/images/` + Technology[RK.TechnologyPN].toLowerCase() + `.svg');"></div>
                                <h2>` + Technology[RK.TechnologyPN] + `</h2>
                                <div class="Separator"></div>
                                <p class="ArticleDescription">` + Technology[RK.DescriptionPN] + `</p>
                            </div>
                            <footer>
                                <p><a href = "` + RK.BASE_URL + `/` + Technology[RK.TechnologyPN].toLowerCase() + `/index.html">Read more</a></p>
                            </footer>
                        </article>`;
                    });
                    break;
                case 404:
                    throw 'Data.json not found';
                    break;
                default:
                    throw 'Unexpected HTTP code ' + XHR.status;
                    break;
            }
        };
        XHR.send(null);
        InnerHTML += `
            </div>
            <h1>Latest news</h1>
            <div style="max-width: 500px; margin: 0px auto;">
                <a class="twitter-timeline" href = "https://twitter.com/RookieHPC?ref_src=twsrc%5Etfw">Tweets by RookieHPC</a>
            </div>
        </section>`;
        RK.GetOutput().innerHTML += InnerHTML;
        const TwitterLink = document.getElementsByClassName('twitter-timeline')[0];
        const TwitterScript = document.createElement('script');
        TwitterLink.parentNode.insertBefore(TwitterScript, TwitterLink.nextSibling);
        TwitterScript.src = "https://platform.twitter.com/widgets.js";
    },

    GenerateAbout: async (Entry) => {
        RK.BuildTechnologyNav(Entry);
        let InnerHTML = `
        <section id="MainSection">
            <div>
                <header>
                    <h1>About ` + Entry[RK.TechnologyPN] + `</h1>
                </header>
                <div>`;
        Entry[RK.SectionsPN].forEach((Section) => {
            InnerHTML += `
            <article class="PageArticle">
                <div class="PageArticleHeader">
                    <h2>` + Section[RK.TitlePN] + `</h2>
                </div>
                <div class="PageArticleBody">
                    <p>` + Section[RK.ContentPN].replaceAll('\n','<br>').replaceAll('\t','&nbsp;&nbsp;&nbsp;&nbsp;') + `</p>
                </div>
            </article>
            `;
        });
        InnerHTML += `</div>
            </div>
        </section>`;
        RK.GetOutput().innerHTML += InnerHTML;
    },

    GenerateToolHomepage: async (Entry) => {
        RK.BuildTechnologyNav(Entry);

        const MainSection = document.createElement('section');
        MainSection.id = "MainSection";
        RK.GetOutput().appendChild(MainSection);

        const MainSectionDiv = document.createElement('div');
        MainSection.appendChild(MainSectionDiv);

        const MainSectionDivHeader = document.createElement('header');
        MainSectionDiv.appendChild(MainSectionDivHeader);

        const ToolHomepageTitle = document.createElement('h1');
        ToolHomepageTitle.innerText = Entry[RK.TechnologyPN] + ' Tools';

        const ToolsDiv = document.createElement('div');
        MainSection.appendChild(ToolsDiv);

        MainSectionDivHeader.appendChild(ToolHomepageTitle);
        Entry[RK.ToolsPN].forEach((Tool) => {
            const ToolDiv = document.createElement('div');
            ToolDiv.classList.add("ToolCard");
            ToolDiv.classList.add("FakeButton");
            ToolDiv.onclick = () => {
                window.location.href = RK.BASE_URL + Tool[RK.UrlPN];
            };
            ToolsDiv.appendChild(ToolDiv);

            const LogoDiv = document.createElement('div');
            LogoDiv.classList.add("logo");
            ToolDiv.appendChild(LogoDiv);

            const LogoImage = document.createElement('div');
            LogoImage.classList.add("LogoImage");
            const ImagePath = RK.BASE_URL + Tool[RK.ImageUrlPN];
            const XHR = new XMLHttpRequest();
            XHR.open("GET", ImagePath, false);
            XHR.onload = () => {
                LogoImage.setAttribute('mask-image', 'url(' + ImagePath + ')');
                LogoImage.style.webkitMaskImage = 'url(' + ImagePath + ')';
            }
            XHR.send(null);
            LogoDiv.appendChild(LogoImage);

            const ToolTitle = document.createElement('h2');
            ToolTitle.classList.add("name");
            ToolDiv.appendChild(ToolTitle);

            const ToolTitleSpan = document.createElement('span');
            ToolTitleSpan.innerText = Tool[RK.NamePN];
            ToolTitle.appendChild(ToolTitleSpan);

            const ToolDescriptionDiv = document.createElement('div');
            ToolDescriptionDiv.classList.add("description");
            ToolDiv.appendChild(ToolDescriptionDiv);

            const ToolDescriptionText = document.createElement('p');
            ToolDescriptionText.innerHTML = Tool[RK.DescriptionPN];
            ToolDescriptionDiv.appendChild(ToolDescriptionText);
        });
    },

    GenerateExerciseHomepage: async (Entry) => {
        RK.BuildTechnologyNav(Entry);

        const MainSection = document.createElement('section');
        MainSection.id = "MainSection";
        RK.GetOutput().appendChild(MainSection);

        const MainSectionDiv = document.createElement('div');
        MainSection.appendChild(MainSectionDiv);

        const MainSectionDivHeader = document.createElement('header');
        MainSectionDiv.appendChild(MainSectionDivHeader);

        const ExerciseHomepageTitle = document.createElement('h1');
        ExerciseHomepageTitle.innerText = Entry[RK.TechnologyPN] + ' Exercises';

        const ToolsDiv = document.createElement('div');
        MainSection.appendChild(ToolsDiv);

        MainSectionDivHeader.appendChild(ExerciseHomepageTitle);

        const DefinitionPage = document.createElement('article');
        DefinitionPage.classList.add("PageArticle")
        MainSection.appendChild(DefinitionPage);

        const DefinitionPageHeader = document.createElement('div');
        DefinitionPageHeader.classList.add("PageArticleHeader")
        DefinitionPage.appendChild(DefinitionPageHeader);

        const DefinitionPageHeaderTitle = document.createElement('h2');
        DefinitionPageHeaderTitle.innerText = 'List of exercises';
        DefinitionPageHeader.appendChild(DefinitionPageHeaderTitle);

        const DefinitionPageBody = document.createElement('div');
        DefinitionPageBody.classList.add("PageArticleBody")
        DefinitionPage.appendChild(DefinitionPageBody);

        const DefinitionPageBodyContent = document.createElement('p');
        DefinitionPageBodyContent.innerHTML = "You want to practice your " + Entry[RK.TechnologyPN] + " skills? This page is made for you. Here is a list of exercises, help yourself and have fun! You would like to suggest a new exercise? Please, contact us (anonymously)!";
        DefinitionPageBody.appendChild(DefinitionPageBodyContent);

        const Table = document.createElement("table");
        Table.id = "ExerciseTable";
        Entry[RK.ExercisesPN].forEach((Exercise) => {
            const Row = document.createElement("tr");
            Row.classList.add("FakeButton");
            Row.onclick = () => {
                window.location.href = "/" + Entry[RK.TechnologyPN] + "/exercises/" + Exercise[RK.DirectoryNamePN] + "/index.html";
            };
            let RowInnerHTML = "<td><span class=\"DifficultyStars\">";
            let I = 0;
            while(I < parseInt(Exercise[RK.DifficultyPN])) {
                RowInnerHTML += "<span class=\"DifficultyStar DifficultyStarFilled\"></span>";
                I++;
            }
            while(I < 3) {
                RowInnerHTML += "<span class=\"DifficultyStar DifficultyStarEmpty\"></span>";
                I++;
            }
            RowInnerHTML += "</span>" + Exercise[RK.NamePN] + "</td>"
            RowInnerHTML += "<td class=\"EllipseableCell\">" + Exercise[RK.DescriptionPN] + "</td>"
            Row.innerHTML = RowInnerHTML;
            Table.appendChild(Row);
        });
        DefinitionPageBody.appendChild(Table);
    },

    GenerateExercise: async (Entry) => {
        return new Promise(async (Resolve, Reject) => {
            await RK.FetchExerciseFilesAndDetectLanguages(Entry);
            Resolve();
        });
    },

    /**
     * @details
     * [
     *     {
     *         "language":"<lang>",
     *         "solution":"<content>",
     *         "provided":"<content>"
     *     }
     * ] 
     * @param {*} Entry 
     *
     */
    FetchExerciseFilesAndDetectLanguages: async (Entry) => {
        return new Promise(async (Resolve, Reject) => {
            Collected = {};
            await RK.FetchExerciseSolutionForLanguage(Entry, Collected, Object.keys(RK.LANGUAGES)[0]);
            Object.keys(Collected).forEach((CollectedLanguage) => {
                RK.CreateCode(Collected[CollectedLanguage][RK.SolutionPN], CollectedLanguage, RK.LANGUAGE_HLJS_CLASS[CollectedLanguage]);
            });
            Resolve();
        });
    },

    FetchExerciseSolutionForLanguage: async (Entry, Collected, LanguageToTry) => {
        return new Promise(async (Resolve, Reject) => {
            const SolutionFilePath = RK.BASE_URL + "/" + Entry[RK.TechnologyPN] + "/exercises/" + Entry[RK.DirectoryNamePN] + "/solution." + RK.LANGUAGE_FILE_EXTENSIONS[LanguageToTry];
            const XHR = new XMLHttpRequest();
            XHR.open("GET", SolutionFilePath);
            XHR.onload = async () => {
                switch(XHR.status) {
                    case 0:
                    case 200:
                        Collected[LanguageToTry] = {
                            "Solution":XHR.responseText,
                            "Provided":""
                        };
                        await RK.FetchExerciseSourceCodeProvidedIfAny(Entry, Collected, LanguageToTry);
                        break;
                    case 404:
                        const LanguageToTryIndex = Object.keys(RK.LANGUAGES).indexOf(LanguageToTry);
                        if((LanguageToTryIndex+1) < Object.keys(RK.LANGUAGES).length) {
                            const LanguageToTryNext = Object.keys(RK.LANGUAGES)[LanguageToTryIndex+1];
                            await RK.FetchExerciseSolutionForLanguage(Entry, Collected, LanguageToTryNext);
                        }
                        else {
                            RK.GenerateExerciseEnd(Entry, Collected);
                        }
                        break;
                    default:
                        break;
                }
                Resolve();
            };
            XHR.send();
        });
    },

    FetchExerciseSourceCodeProvidedIfAny: async (Entry, Collected, LanguageToTry) => {
        return new Promise(async (Resolve, Reject) => {
            const SolutionFilePath = RK.BASE_URL + "/" + Entry[RK.TechnologyPN] + "/exercises/" + Entry[RK.DirectoryNamePN] + "/provided." + RK.LANGUAGE_FILE_EXTENSIONS[LanguageToTry];
            const XHR = new XMLHttpRequest();
            XHR.open("GET", SolutionFilePath);
            XHR.onload = async () => {
                switch(XHR.status) {
                    case 0:
                    case 200:
                        Collected[LanguageToTry][RK.ProvidedPN] = XHR.responseText;
                        break;
                    case 404:
                        break;
                    default:
                        break;
                }
                const LanguageToTryIndex = Object.keys(RK.LANGUAGES).indexOf(LanguageToTry);
                if((LanguageToTryIndex+1) < Object.keys(RK.LANGUAGES).length) {
                    const LanguageToTryNext = Object.keys(RK.LANGUAGES)[LanguageToTryIndex+1];
                    await RK.FetchExerciseSolutionForLanguage(Entry, Collected, LanguageToTryNext);
                }
                else {
                    RK.GenerateExerciseEnd(Entry, Collected);
                }
                Resolve();
            };
            XHR.send();
        });
    },

    GenerateExerciseEnd: (Entry, Collected) => {
        RK.BuildTechnologyNav(Entry);

        const MainSection = document.createElement('section');
        MainSection.id = "MainSection";
        RK.GetOutput().appendChild(MainSection);

        const MainSectionDiv = document.createElement('div');
        MainSection.appendChild(MainSectionDiv);

        ////////////////////
        // Difficulty div //
        ////////////////////
        const DifficultyDiv = document.createElement('div');
        DifficultyDiv.classList.add("DifficultyWidget");
        MainSectionDiv.appendChild(DifficultyDiv);

        const DifficultyLabel = document.createElement('p');
        DifficultyDiv.appendChild(DifficultyLabel);

        const DifficultyLabelTopLevelSpan = document.createElement('span');
        DifficultyLabelTopLevelSpan.classList.add("DifficultyStars");
        DifficultyLabel.appendChild(DifficultyLabelTopLevelSpan);

        const DifficultyLabelSecondTopLevelSpan = document.createElement('span');
        DifficultyLabelSecondTopLevelSpan.classList.add("DifficultyStars");
        DifficultyLabelTopLevelSpan.appendChild(DifficultyLabelSecondTopLevelSpan);

        for(let i = 0; i < parseInt(Entry[RK.DifficultyPN]); i++) {
            const DifficultyStarSpan = document.createElement("span");
            DifficultyStarSpan.classList.add("DifficultyStar", "DifficultyStarFilled");
            DifficultyLabelSecondTopLevelSpan.appendChild(DifficultyStarSpan);
        }
        for(let i = parseInt(Entry[RK.DifficultyPN]); i < 3; i++) {
            const DifficultyStarSpan = document.createElement("span");
            DifficultyStarSpan.classList.add("DifficultyStar", "DifficultyStarEmpty");
            DifficultyLabelSecondTopLevelSpan.appendChild(DifficultyStarSpan);
        }
        DifficultyLabelTopLevelSpan.appendChild(document.createTextNode(RK.DIFFICULTY[Entry[RK.DifficultyPN]] + " difficulty"));

        //////////////////
        // Version tabs //
        //////////////////
        const VersionTabsDiv = document.createElement('div');
        VersionTabsDiv.id = "VersionTabs";
        MainSectionDiv.appendChild(VersionTabsDiv);

        const VersionTabsP = document.createElement('p');
        VersionTabsDiv.appendChild(VersionTabsP);

        let FirstVersionTab = true;
        Object.keys(Collected).forEach((CollectedLanguage) => {
            if(FirstVersionTab === false) {
                VersionTabsP.appendChild(document.createTextNode(" | "));
            }
            else {
                FirstVersionTab = false;
            }
            const VersionTabSpan = document.createElement('span');
            VersionTabSpan.classList.add("VersionTab", "FakeButton");
            VersionTabSpan.onclick = () => {
                RK.ShowVersion(Event, RK.LANGUAGES[CollectedLanguage]);
            }
            VersionTabSpan.textContent = RK.LANGUAGES[CollectedLanguage];
            VersionTabsP.appendChild(VersionTabSpan);
        });

        ////////////
        // Header //
        ////////////
        const MainSectionDivHeader = document.createElement('header');
        MainSectionDiv.appendChild(MainSectionDivHeader);

        const ExerciseHomepageTitle = document.createElement('h1');
        ExerciseHomepageTitle.innerText = Entry[RK.TitlePN];

        const ToolsDiv = document.createElement('div');
        MainSection.appendChild(ToolsDiv);

        MainSectionDivHeader.appendChild(ExerciseHomepageTitle);

        const DefinitionPage = document.createElement('article');
        DefinitionPage.classList.add("PageArticle")
        MainSectionDiv.appendChild(DefinitionPage);

        const DefinitionPageHeader = document.createElement('div');
        DefinitionPageHeader.classList.add("PageArticleHeader")
        DefinitionPage.appendChild(DefinitionPageHeader);

        const DefinitionPageHeaderTitle = document.createElement('h2');
        DefinitionPageHeaderTitle.innerText = 'Introduction';
        DefinitionPageHeader.appendChild(DefinitionPageHeaderTitle);

        const DefinitionPageBody = document.createElement('div');
        DefinitionPageBody.classList.add("PageArticleBody")
        DefinitionPage.appendChild(DefinitionPageBody);

        const DefinitionPageBodyContent = document.createElement('p');
        DefinitionPageBodyContent.innerHTML = Entry[RK.IntroductionPN];
        DefinitionPageBody.appendChild(DefinitionPageBodyContent);

        // Get output
        const XHR = new XMLHttpRequest();
        const OutputFilePath = RK.BASE_URL + "/" + Entry[RK.TechnologyPN] + "/exercises/" + Entry[RK.DirectoryNamePN] + "/output.txt";
        XHR.open("GET", OutputFilePath);
        XHR.onload = () => {
            switch(XHR.status) {
                case 0:
                case 200:
                    const OutputFilePage = document.createElement('article');
                    OutputFilePage.classList.add("PageArticle")
                    MainSectionDiv.appendChild(OutputFilePage);

                    const OutputPageHeader = document.createElement('div');
                    OutputPageHeader.classList.add("PageArticleHeader")
                    OutputFilePage.appendChild(OutputPageHeader);

                    const OutputPageHeaderTitle = document.createElement('h2');
                    OutputPageHeaderTitle.innerText = 'Expected output';
                    OutputPageHeader.appendChild(OutputPageHeaderTitle);

                    const OutputPageBody = document.createElement('div');
                    OutputPageBody.classList.add("PageArticleBody")
                    OutputFilePage.appendChild(OutputPageBody);

                    const ConsoleOutput = document.createElement('div');
                    ConsoleOutput.classList.add("ConsoleOutput");
                    OutputPageBody.appendChild(ConsoleOutput);

                    const OutputText = XHR.responseText;
                    const OutputTextLines = OutputText.split('\n');
                    OutputTextLines.forEach((OutputTextLine) => {
                        // TODO Do something with that line
                        const ConsoleOutputLine = document.createElement('pre');
                        ConsoleOutputLine.classList.add("ConsoleOutputLine");
                        ConsoleOutputLine.textContent = OutputTextLine;
                        ConsoleOutput.appendChild(ConsoleOutputLine);
                    });
                    break;
                case 404:
                    break;
                default:
                    alert("Output file loading: unexpected XHR code " + XHR.status);
            }

            // Hints
            const HintPage = document.createElement('article');
            HintPage.classList.add("PageArticle")
            MainSectionDiv.appendChild(HintPage);

            const OutputPageHeader = document.createElement('div');
            OutputPageHeader.classList.add("PageArticleHeader")
            HintPage.appendChild(OutputPageHeader);

            const OutputPageHeaderTitle = document.createElement('h2');
            OutputPageHeaderTitle.innerText = 'Hints';
            OutputPageHeader.appendChild(OutputPageHeaderTitle);

            const OutputPageBody = document.createElement('div');
            OutputPageBody.classList.add("PageArticleBody")
            HintPage.appendChild(OutputPageBody);

            const OutputPageBodyContent = document.createElement('p');
            OutputPageBodyContent.innerHTML = RK.InterpretMarkdown(RK.InsertCrossReferencesFromTechnology(Entry[RK.HintsPN])) || "None: you are on your own on this one.";
            OutputPageBody.appendChild(OutputPageBodyContent);

            // Create source code sections
            Object.keys(RK.LANGUAGES).forEach((Language) => {
                if(Collected.hasOwnProperty(Language)) {
                    const CollectedEntry = Collected[Language];

                    // Source code provided
                    const OutputPage = document.createElement('article');
                    OutputPage.classList.add("PageArticle", "Version", RK.LANGUAGES[Language]);
                    MainSectionDiv.appendChild(OutputPage);

                    const OutputPageHeader = document.createElement('div');
                    OutputPageHeader.classList.add("PageArticleHeader");
                    OutputPage.appendChild(OutputPageHeader);

                    const OutputPageHeaderTitle = document.createElement('h2');
                    OutputPageHeaderTitle.innerText = 'Source code provided';
                    OutputPageHeader.appendChild(OutputPageHeaderTitle);

                    const OutputPageBody = document.createElement('div');
                    OutputPageBody.classList.add("PageArticleBody")
                    OutputPage.appendChild(OutputPageBody);
                    if(CollectedEntry[RK.ProvidedPN] === "") {
                        const NoSourceCodeProvidedLabel = document.createElement('p');
                        NoSourceCodeProvidedLabel.textContent = "None: you have to write it from scratch.";
                        OutputPageBody.appendChild(NoSourceCodeProvidedLabel);
                    } else {
                        RK.CreateCode(CollectedEntry[RK.ProvidedPN], OutputPageBody, RK.LANGUAGE_HLJS_CLASS[Language]);
                    }

                    // Solution
                    const SolutionPage = document.createElement('article');
                    SolutionPage.classList.add("PageArticle", "Version", RK.LANGUAGES[Language]);
                    MainSectionDiv.appendChild(SolutionPage);

                    const SolutionPageHeader = document.createElement('div');
                    SolutionPageHeader.classList.add("PageArticleHeader");
                    SolutionPage.appendChild(SolutionPageHeader);

                    const SolutionPageHeaderTitle = document.createElement('h2');
                    SolutionPageHeaderTitle.innerText = 'Solution';
                    SolutionPageHeader.appendChild(SolutionPageHeaderTitle);

                    const SolutionPageBody = document.createElement('div');
                    SolutionPageBody.classList.add("PageArticleBody")
                    SolutionPage.appendChild(SolutionPageBody);
                    RK.CreateCode(CollectedEntry[RK.SolutionPN], SolutionPageBody, RK.LANGUAGE_HLJS_CLASS[Language]);
                }
            });
            
            // Trigger first language show
            // TODO Use some cookie
            document.getElementsByClassName("VersionTab")[0].click();
        };
        XHR.send(null);
    },

    GenerateText: async (Entry) => {
        let InnerHTML = `
        <section id="MainSection">
            <div>
                <header>
                    <h1>` + Entry[RK.TitlePN] + `</h1>
                </header>
                <div>`;
        Entry[RK.SectionsPN].forEach((Section) => {
            InnerHTML += `
            <article class="PageArticle">
                <div class="PageArticleHeader">
                    <h2>` + Section[RK.TitlePN] + `</h2>
                </div>
                <div class="PageArticleBody">
                    <p>` + Section[RK.ContentPN].replaceAll('\n','<br>').replaceAll('\t','&nbsp;&nbsp;&nbsp;&nbsp;') + `</p>
                </div>
            </article>
            `;
        });
        InnerHTML += `</div>
            </div>
        </section>`;
        RK.GetOutput().innerHTML += InnerHTML;
    },

    Generate: async (Entry, SelectedOutput) => {
        return new Promise(async (Resolve, Reject) =>{
            try {
                RK.Verify(Entry);
                const Root = document.documentElement;
                Root.style.setProperty('--FeedbackImagePath', "url(" + RK.BASE_URL + "/images/bubbleBlack.svg)");
                Root.style.setProperty('--CopyImagePath', "url(" + RK.BASE_URL + "/images/copyBlack.svg)");

                RK.SetOutput(SelectedOutput);
                RK.GetOutput().innerText = "";
                // Will automatically call for generation of body, then footer etc...
                await RK.BuildHeader(Entry);
                switch(Entry[RK.TypePN]) {
                    case RK.ENTRY_TYPES.TEXT:
                        await RK.GenerateText(Entry);
                        break;
                    case RK.ENTRY_TYPES.ABOUT:
                        await RK.GenerateAbout(Entry);
                        break;
                    case RK.ENTRY_TYPES.DOCUMENTATION:
                        await RK.GenerateDocumentation(Entry);
                        break;
                    case RK.ENTRY_TYPES.TOOL_HOMEPAGE:
                        await RK.GenerateToolHomepage(Entry);
                        break;
                    case RK.ENTRY_TYPES.TOOL:
                        throw 'Unimplemented yet';
                        break;
                    case RK.ENTRY_TYPES.EXERCISE_HOMEPAGE:
                        await RK.GenerateExerciseHomepage(Entry);
                        break;
                    case RK.ENTRY_TYPES.EXERCISE:
                        await RK.GenerateExercise(Entry);
                        break;
                    case RK.ENTRY_TYPES.DOCUMENTATION_HOMEPAGE:
                        await RK.GenerateDocumentationHomepage(Entry);
                        break;
                    case RK.ENTRY_TYPES.HOMEPAGE:
                        await RK.GenerateHomepage(Entry);
                        break;
                    default:
                        throw 'Unsupported entry type: "' + Entry[RK.TypePN] + '".';
                        break;
                }
                RK.BuildFooter(Entry);
                RK.LoadingFinished();
                Resolve();
            }
            catch(e) {
                throw RK.LocationOpener + "Error at " + RK.GetLocation() + RK.LocationCloser + e;
            }
        });
    },

    CreateCode: (SourceCode, Element, LanguageClass) => {
        Element.innerHTML += `
        <div class="SourceCodeViewer">
            <div class="SourceCodeToolbar">
                <div class="FakeButton NonUserSelectable SourceCodeToolbarButton">
                    <div class="SourceCodeToolbarButtonIcon SourceCodeToolbarButtonIconFeedback SourceCodeToolbarButton">
                    </div>
                    <p class="SourceCodeToolbarButtonText">Feedback</p>
                </div>
            </div>
            <pre class="SourceCode"><code>` + hljs.lineNumbersValue(RK.InsertCrossReferencesFromTechnology(hljs.highlight(SourceCode, {language: LanguageClass}).value)) + `</code>
            </pre>
        </div>`;
    },

    EscapeCode: (UnsafeString) => {
        return UnsafeString.replace(/&/g, "&amp;")
                            .replace(/</g, "&lt;")
                            .replace(/>/g, "&gt;")
                            .replace(/"/g, "&quot;")
                            .replace(/'/g, "&#039;");
    },

    ChangeSearchState: () => {
        const SearchIcon = document.getElementById('SearchIcon');
        if(!SearchIcon.hasAttribute('data-searchlaunched')) {
            SearchIcon.setAttribute('data-searchlaunched', true);
            SearchIcon.style.backgroundImage = "url('" + RK.BASE_URL + "/images/cross.svg')";
            document.getElementById("ArticleLinks").style.border = "1px solid rgb(220, 220, 220)";
            RK.LaunchSearch();
            document.addEventListener('click', RK.CloseListIfClickedOutside);
            document.getElementById('SearchBar').style.borderBottomLeftRadius = '0px';
        }
        else {
            RK.HideAllArticleLinks();
            document.getElementById('NoResultFound').style.display = "none";
            document.getElementById("SearchBar").value = "";
            window.scrollTo(0, 0);
            SearchIcon.style.backgroundImage = "url('" + RK.BASE_URL + "/images/magnifyingGlass.svg')";
            SearchIcon.removeAttribute('data-searchlaunched');
            document.getElementById("ArticleLinks").style.border = "none";
            document.removeEventListener('click', RK.CloseListIfClickedOutside);
            document.getElementById('SearchBar').style.borderBottomLeftRadius = '5px';
        }
    },

    CloseListIfClickedOutside: (Event) => {
        Event = Event || window.Event;
        const Target = Event.target || Event.srcElement;
        if(!document.getElementById('SearchForm').contains(Target))
        {
            RK.ChangeSearchState();
        }
    },

    ResizeSearchResultList: () => {
        const SearchBar = document.getElementById('SearchBar');
        const CompStyles = window.getComputedStyle(SearchBar);
        const SearchBarWidth = parseInt(CompStyles.getPropertyValue('width').split("px")[0]);

        const ArticleLinks = document.getElementById('ArticleLinks');
        ArticleLinks.style.width = SearchBarWidth + "px";
    },

    HideAllArticleLinks: () => {
        const AllArticleLinks = RK.GetAllArticleLinks();
        for(var i = 0, max = AllArticleLinks.length; i < max; i++)
        {
            AllArticleLinks[i].style.display = "none";
        }
    },
    
    GetAllArticleLinks: () => {
        return document.getElementsByClassName("ArticleLink");
    },

    LaunchSearch: () => {
        const selectionField = document.getElementById("SearchBar");
        const selectionString = selectionField.value;
        selectionStringLowerCase = selectionString.toLowerCase();

        let AtLeastOneResult = false;
        const AllArticleLinks = RK.GetAllArticleLinks();
        for(var i = 0, max = AllArticleLinks.length; i < max; i++) {
            const CurrentArticleLinkLi = AllArticleLinks[i];
            const CurrentArticleLink = CurrentArticleLinkLi.getElementsByClassName('ArticleLinkName')[0];

            let OldInnerHTML = CurrentArticleLink.innerHTML;
            OldInnerHTML = OldInnerHTML.replace(/<strong>/g, "");
            OldInnerHTML = OldInnerHTML.replace(/<\/strong>/g, "");
            OldInnerHTMLLowerCase = OldInnerHTML.toLowerCase();

            if(OldInnerHTMLLowerCase.includes(selectionStringLowerCase.toLowerCase())) {
                AtLeastOneResult = true;
                CurrentArticleLinkLi.style.display = "block";
                if(OldInnerHTMLLowerCase.indexOf(selectionStringLowerCase) != -1) {
                    CurrentArticleLink.innerHTML = OldInnerHTML.substr(0, OldInnerHTMLLowerCase.indexOf(selectionStringLowerCase));
                    CurrentArticleLink.innerHTML += "<strong>" + OldInnerHTML.substr(OldInnerHTMLLowerCase.indexOf(selectionStringLowerCase), selectionStringLowerCase.length) + "</strong>";
                    CurrentArticleLink.innerHTML += OldInnerHTML.substr(OldInnerHTMLLowerCase.indexOf(selectionStringLowerCase) + selectionStringLowerCase.length);
                }
            }
            else {
                CurrentArticleLinkLi.style.display = "none";
            }
        }

        const NoResultFound = document.getElementById('NoResultFound');

        if(AtLeastOneResult == false) {
            NoResultFound.style.display = "block";
            NoResultFound.innerHTML = "<a href='mailto:" + RK.CONTACT_EMAIL + "?subject=Missing documentation&body=Hi, I found that the documentation may be missing \"" + selectionString + "\". Could you please have a look?'><span class='ArticleLinkTechnology'>Unknown</span><span class='ArticleLinkName'>No result found. Is <b>\"" + selectionString + "\"</b> missing? Tell us (anonymously) just by clicking on this text.</span></a>";
            RK.ResizeSearchResultList();
        }
        else {
            NoResultFound.style.display = "none";
        }
    },

    BuildHeader: (Entry) => {
        return new Promise((Resolve, Reject) => {
            const MainHeader = document.createElement("div");
            MainHeader.id = "MainHeader";
            RK.GetOutput().appendChild(MainHeader);

            InnerHTML = `
                <img id="MainLogo" src="` + RK.BASE_URL + `/images/logoWhite.svg" width="40" onclick="window.location='` + RK.BASE_URL + `/index.html'" alt="Logo of RookieHPC">
                <div id="SearchArea">
                    <div id="SearchForm">
                        <input type="text" id="SearchBar" name="SearchBar" autocomplete="off"><span id="SearchIcon" class="FakeButton" style="background-image: url('` + RK.BASE_URL + `/images/magnifyingGlass.svg')";></span>
                    </div>
                    <ul id="ArticleLinks" style="width: 909px;">
                        <li id="NoResultFound" class="NonUserSelectable FakeButton"></li>
                    </ul>
                </div>
                <script>
                    var SearchBarElement = document.getElementById('SearchBar');
                    var ArticleLinksElement = document.getElementById('ArticleLinks');
                    ArticleLinksElement.style.width = SearchBarElement.clientWidth + "px";
                </script>
                <p id="NavigationPath"><a href = "` + RK.BASE_URL + `/index.html">Homepage</a>`;
            if(Entry[RK.TypePN] == RK.ENTRY_TYPES.HOMEPAGE)
            {
                // Nothing else to add to the navigation path
            }
            else if(Entry[RK.TypePN] == RK.ENTRY_TYPES.TEXT) {
                InnerHTML += `&nbsp; ▸ &nbsp;<a href = "` + RK.BASE_URL + `/` + Entry[RK.DirectoryNamePN].toLowerCase() + `/index.html">` + Entry[RK.NamePN] + `</a>`;
            }
            else
            {
                InnerHTML += `&nbsp; ▸ &nbsp;<a href = "` + RK.BASE_URL + `/` + Entry[RK.TechnologyPN].toLowerCase() + `/index.html">` + Entry[RK.TechnologyPN] + `</a>`;
                let TypeString = "";
                switch(Entry[RK.TypePN]) {
                    case RK.ENTRY_TYPES.DOCUMENTATION_HOMEPAGE:
                    case RK.ENTRY_TYPES.DOCUMENTATION:
                        TypeString = "Docs";
                        break;
                    case RK.ENTRY_TYPES.TOOL_HOMEPAGE:
                    case RK.ENTRY_TYPES.TOOL:
                        TypeString = "Tools";
                        break;
                    case RK.ENTRY_TYPES.EXERCISE_HOMEPAGE:
                    case RK.ENTRY_TYPES.EXERCISE:
                        TypeString = "Exercises";
                        break;                        
                }
                InnerHTML += `&nbsp; ▸ &nbsp;<a href = "` + RK.BASE_URL + `/` + Entry[RK.TechnologyPN].toLowerCase() + `/` + TypeString.toLowerCase() + `/index.html">` + TypeString + `</a>`;
                if(Entry[RK.TypePN] == RK.ENTRY_TYPES.DOCUMENTATION || 
                Entry[RK.TypePN] == RK.ENTRY_TYPES.EXERCISE || 
                Entry[RK.TypePN] == RK.ENTRY_TYPES.TOOL) {
                    InnerHTML += `&nbsp; ▸ &nbsp;<a href = "` + RK.BASE_URL + `/` + Entry[RK.TechnologyPN].toLowerCase() + `/` + TypeString.toLowerCase() + `/` + Entry[RK.DirectoryNamePN] + `/index.html">` + Entry[RK.NamePN] + `</a>`;
                }
            }
            InnerHTML += `</p>`;
            MainHeader.innerHTML = InnerHTML;
            const XHR = new XMLHttpRequest();
            const ManifestPath = RK.BASE_URL + "/" + RK.LOOKUP_NAME;
            XHR.open("GET", ManifestPath);
            XHR.onload = () => {
                switch(XHR.status) {
                    case 0:
                    case 200:
                        const ArticleLinks = document.getElementById("ArticleLinks");
                        window.Manifest = JSON.parse(XHR.responseText);
                        window.Manifest.forEach((TechnologyEntry) => {
                            TechnologyEntry[RK.EntriesPN].forEach((DocumentationEntry) => {
                                ArticleLinks.innerHTML += `<li class="ArticleLink NonUserSelectable FakeButton"><a href = "` + RK.BASE_URL + TechnologyEntry['PathRoot'] + `/` + DocumentationEntry['DirectoryName'] + `/index.html"><span class="ArticleLinkTechnology">` + TechnologyEntry[RK.TechnologyPN] + `</span><span class="ArticleLinkName">` + DocumentationEntry['Name'] + `</span></a></li>`
                            })
                        });
                        Resolve();
                        break;
                    case 404:
                        alert("Manifest not found at \"" + ManifestPath + "\"");
                        Reject();
                        break;
                    default:
                        alert("Manifest loading: unexpected XHR code " + XHR.status);
                        Reject();
                        break;
                }
            };
            XHR.send(null);
        });
    },

    /**
     * @brief Builds the subheader roll that contains the about / docs / tools / exercise tabs.
     * @todo Use the entry parameter to determine which tab is selected
     **/
    BuildTechnologyNav: (Entry) => {
        const SubHeader = document.createElement("div");
        SubHeader.id = "SubHeader";
        RK.GetOutput().appendChild(SubHeader);
        SubHeader.innerHTML = `
            <nav id="categoryList">
                <a class="CategoryItem FakeButton" href = "` + RK.BASE_URL + `/` + Entry[RK.TechnologyPN].toLowerCase() + `/index.html" title="The page containing a more detailed description about the ` + Entry[RK.TechnologyPN] + ` technology.">
                    <div class="Logo" style="mask-image: url('` + RK.BASE_URL + `/images/about.svg');-webkit-mask-image: url('` + RK.BASE_URL + `/images/about.svg');"></div>
                    <span class="Description">About</span>
                </a><!--
                --><a class="CategoryItem FakeButton" href = "` + RK.BASE_URL + `/` + Entry[RK.TechnologyPN].toLowerCase() + `/docs/index.html" title="The page covering the documentation for the ` + Entry[RK.TechnologyPN] + ` technology, supported with examples.">
                    <div class="Logo" style="mask-image: url('` + RK.BASE_URL + `/images/bookClosed.svg');-webkit-mask-image: url('` + RK.BASE_URL + `/images/bookClosed.svg');"></div>
                    <span class="Description">Docs</span>
                </a><!--
                --><a class="CategoryItem FakeButton" href = "` + RK.BASE_URL + `/` + Entry[RK.TechnologyPN].toLowerCase() + `/tools/index.html" title="The page listing the tools you can access to improve your learning and development of applications that use ` + Entry[RK.TechnologyPN] + `.">
                    <div class="Logo" style="mask-image: url('` + RK.BASE_URL + `/images/tools.svg');-webkit-mask-image: url('` + RK.BASE_URL + `/images/tools.svg');"></div>
                    <span class="Description">Tools</span>
                </a><!--
                --><a class="CategoryItem FakeButton" href = "` + RK.BASE_URL + `/` + Entry[RK.TechnologyPN].toLowerCase() + `/exercises/index.html" title="The page testing your knowledge of ` + Entry[RK.TechnologyPN] + ` with a list of exercises.">
                    <div class="Logo" style="mask-image: url('` + RK.BASE_URL + `/images/exercises.svg');-webkit-mask-image: url('` + RK.BASE_URL + `/images/exercises.svg');"></div>
                    <span class="Description">Exercises</span>
                </a>
            </nav>`;
    },

    BuildFooter: () => {
        const Footer = document.createElement("footer");
        Footer.id = "MainFooter";
        RK.GetOutput().appendChild(Footer);
        Footer.innerHTML = `
        	<p id="FooterLogo"><a href = "` + RK.BASE_URL + `/index.html"><img src="` + RK.BASE_URL + `/images/logoAndTitleBlack.svg" alt="Logo of the Rookie HPC website."></a></p>
        	<ul>
                <li><a href = "` + RK.BASE_URL + `/editor.html?src=` + RK.FolderPath + `" title="Edit the page using the RookieHPC editor.">Edit this page</a></li>
        		<li><a href = "` + RK.BASE_URL + `/sitemap/index.html">Site Map</a></li>
        		<li><a href = "` + RK.BASE_URL + `/privacy_policy/index.html">Privacy Policy</a></li>
        		<li><a href = "https://github.com/rookiehpc">Contact</a></li>
        		<li><a href = "https://www.twitter.com/RookieHPC"><img class="SocialNetworks" src="` + RK.BASE_URL + `/images/twitter.svg" alt="Logo of the Twitter account for Rookie HPC."></a></li>
        	</ul>
        	<p id='Copyright'>2019-` + new Date().getFullYear() + ` RookieHPC</p>`;
    },

    Build: () => {
        RK.LoadingStarted();

        // Load favicon
        let NewFaviconLink = document.createElement('link');
        NewFaviconLink.rel = "apple-touch-icon";
        NewFaviconLink.sizes = "57x57";
        NewFaviconLink.href = RK.BASE_URL + "/images/favicons/apple-icon-57x57.png";
        document.head.appendChild(NewFaviconLink);

        NewFaviconLink = document.createElement('link');
        NewFaviconLink.rel = "apple-touch-icon";
        NewFaviconLink.sizes = "60x60";
        NewFaviconLink.href = RK.BASE_URL + "/images/favicons/apple-icon-60x60.png";
        document.head.appendChild(NewFaviconLink);

        NewFaviconLink = document.createElement('link');
        NewFaviconLink.rel = "apple-touch-icon";
        NewFaviconLink.sizes = "72x72";
        NewFaviconLink.href = RK.BASE_URL + "/images/favicons/apple-icon-72x72.png";
        document.head.appendChild(NewFaviconLink);

        NewFaviconLink = document.createElement('link');
        NewFaviconLink.rel = "apple-touch-icon";
        NewFaviconLink.sizes = "76x76";
        NewFaviconLink.href = RK.BASE_URL + "/images/favicons/apple-icon-76x76.png";
        document.head.appendChild(NewFaviconLink);

        NewFaviconLink = document.createElement('link');
        NewFaviconLink.rel = "apple-touch-icon";
        NewFaviconLink.sizes = "114x114";
        NewFaviconLink.href = RK.BASE_URL + "/images/favicons/apple-icon-114x114.png";
        document.head.appendChild(NewFaviconLink);

        NewFaviconLink = document.createElement('link');
        NewFaviconLink.rel = "apple-touch-icon";
        NewFaviconLink.sizes = "120x120";
        NewFaviconLink.href = RK.BASE_URL + "/images/favicons/apple-icon-120x120.png";
        document.head.appendChild(NewFaviconLink);

        NewFaviconLink = document.createElement('link');
        NewFaviconLink.rel = "apple-touch-icon";
        NewFaviconLink.sizes = "144x144";
        NewFaviconLink.href = RK.BASE_URL + "/images/favicons/apple-icon-144x144.png";
        document.head.appendChild(NewFaviconLink);

        NewFaviconLink = document.createElement('link');
        NewFaviconLink.rel = "apple-touch-icon";
        NewFaviconLink.sizes = "152x152";
        NewFaviconLink.href = RK.BASE_URL + "/images/favicons/apple-icon-152x152.png";
        document.head.appendChild(NewFaviconLink);

        NewFaviconLink = document.createElement('link');
        NewFaviconLink.rel = "apple-touch-icon";
        NewFaviconLink.sizes = "180x180";
        NewFaviconLink.href = RK.BASE_URL + "/images/favicons/apple-icon-180x180.png";
        document.head.appendChild(NewFaviconLink);

        NewFaviconLink = document.createElement('link');
        NewFaviconLink.rel = "icon";
        NewFaviconLink.type = "image/png";
        NewFaviconLink.sizes = "192x192";
        NewFaviconLink.href = RK.BASE_URL + "/images/favicons/android-icon-192x192.png";
        document.head.appendChild(NewFaviconLink);

        NewFaviconLink = document.createElement('link');
        NewFaviconLink.rel = "icon";
        NewFaviconLink.type = "image/png";
        NewFaviconLink.sizes = "32x32";
        NewFaviconLink.href = RK.BASE_URL + "/images/favicons/favicon-32x32.png";
        document.head.appendChild(NewFaviconLink);

        NewFaviconLink = document.createElement('link');
        NewFaviconLink.rel = "icon";
        NewFaviconLink.type = "image/png";
        NewFaviconLink.sizes = "96x96";
        NewFaviconLink.href = RK.BASE_URL + "/images/favicons/favicon-96x96.png";
        document.head.appendChild(NewFaviconLink);

        NewFaviconLink = document.createElement('link');
        NewFaviconLink.rel = "icon";
        NewFaviconLink.type = "image/png";
        NewFaviconLink.sizes = "16x16";
        NewFaviconLink.href = RK.BASE_URL + "/images/favicons/favicon-16x16.png";
        document.head.appendChild(NewFaviconLink);

        NewFaviconLink = document.createElement('link');
        NewFaviconLink.rel = "manifest";
        NewFaviconLink.href = RK.BASE_URL + "/images/favicons/manifest.json";
        document.head.appendChild(NewFaviconLink);

        NewFaviconMeta = document.createElement('meta');
        NewFaviconMeta.name = "msapplication-TileColor";
        NewFaviconMeta.content = "#ffffff";
        document.head.appendChild(NewFaviconMeta);

        NewFaviconMeta = document.createElement('meta');
        NewFaviconMeta.name = "msapplication-TileImage";
        NewFaviconMeta.content = RK.BASE_URL + "/images/favicons/ms-icon-144x144.png";
        document.head.appendChild(NewFaviconMeta);

        // Load the HighlightJs JS
        const PathToHighlighJs = RK.BASE_URL + "/highlight/highlight.min.js";
        const HighlighJsScript = document.createElement('script');
        document.body.appendChild(HighlighJsScript);
        HighlighJsScript.onload = RK.HighlighJsScriptLoaded;
        HighlighJsScript.src = PathToHighlighJs;
    },
    
    HighlighJsScriptLoaded: () => { 
        // Load the HighlighJs CSS     
        const PathToHighlighCss = RK.BASE_URL + "/highlight/styles/default.min.css";  
        const HighlighJsCss = document.createElement('link');
        document.head.appendChild(HighlighJsCss);
        HighlighJsCss.onload = RK.HighlighJsCssLoaded;
        HighlighJsCss.rel = "stylesheet";
        HighlighJsCss.href = PathToHighlighCss;
    },

    HighlighJsCssLoaded: () => {
        // Load the HighlighJs line numbering plugin
        const PathToHighlighJsLineNumbering = RK.BASE_URL + "/highlightjs-line-numbers.min.js";
        const HighlighJsLineNumbering = document.createElement('script');
        document.body.appendChild(HighlighJsLineNumbering);
        HighlighJsLineNumbering.onload = RK.HighlighJsLineNumberingLoaded;
        HighlighJsLineNumbering.src = PathToHighlighJsLineNumbering;
    },

    HighlighJsLineNumberingLoaded: () => {
        // Load the common CSS
        const PathToCommonCss = RK.BASE_URL + "/common.css";
        const CommonCss = document.createElement('link');
        document.head.appendChild(CommonCss);
        CommonCss.onload = RK.CommonCssLoaded;
        CommonCss.rel = "stylesheet";
        CommonCss.href = PathToCommonCss;
    },

    CommonCssLoaded: () => {
        // Load the common docs CSS
        const PathToCommonDocsCss = RK.BASE_URL + "/common_docs.css";
        const CommonDocsCss = document.createElement('link');
        document.head.appendChild(CommonDocsCss);
        CommonDocsCss.onload = RK.CommonDocsCssLoaded;
        CommonDocsCss.rel = "stylesheet";
        CommonDocsCss.href = PathToCommonDocsCss;
    },

    CommonDocsCssLoaded: () => {
        // Load the common tools CSS
        const PathToCommonToolsCss = RK.BASE_URL + "/common_tools.css";
        const CommonToolsCss = document.createElement('link');
        document.head.appendChild(CommonToolsCss);
        CommonToolsCss.onload = RK.CommonToolsCssLoaded;
        CommonToolsCss.rel = "stylesheet";
        CommonToolsCss.href = PathToCommonToolsCss;
    },

    CommonToolsCssLoaded: () => {
        // Load the common exercises CSS
        const PathToCommonExercisesCss = RK.BASE_URL + "/common_exercises.css";
        const CommonExercisesCss = document.createElement('link');
        document.head.appendChild(CommonExercisesCss);
        CommonExercisesCss.onload = RK.CommonExercisesCssLoaded;
        CommonExercisesCss.rel = "stylesheet";
        CommonExercisesCss.href = PathToCommonExercisesCss;
    },

    CommonExercisesCssLoaded: () => {
        // Load the fonts script
        const PathToLoadFontsScript = RK.BASE_URL + "/load_fonts.js";
        const LoadFontsScript = document.createElement('script');
        document.body.appendChild(LoadFontsScript);
        LoadFontsScript.onload = RK.LoadFontsScriptLoaded;
        LoadFontsScript.src = PathToLoadFontsScript;
    },

    LoadFontsScriptLoaded: () => {
        const XHR = new XMLHttpRequest();
        XHR.open("GET", RK.FolderPath + "/data.json", true);
        XHR.onload = () => {
            RK.BuildProcess(XHR);
        };
        XHR.send(null);
    },

    BuildProcess: (XHR) => {
        switch(XHR.status) {
            case 200:
                RK.Generate(JSON.parse(XHR.responseText), document.body);
                break;
            case 404:
                alert("Data.json file not found.");
                break;
            default:
                alert("Unexpected code " + XHR.status);
                break;
        }
    },

    LoadingStarted: () => {
        document.body.innerHTML += `
        <style>body{margin:0;padding:0;}#Loader{position:absolute;display:block;width:100vw;height:100vh;background-color:var(--BackgroundColour);z-index:1;}#SpinnerContainer{position:absolute;display:block;left:50%;top:50%;transform:translate(-50%,-50%);color:var(--Colour);font-family:Monaco;animation-name:fading;animation-duration:1s}#Spinner{display:block;margin:auto;width:100px;height:100px;border:16px solid var(--WeakBackgroundColour);border-top:16px solid var(--SpecialColour);border-radius:50%;animation:spin 2s linear infinite}@keyframes spin{0%{transform:rotate(0)}100%{transform:rotate(360deg)}}@keyframes fading{0%{opacity: 0}100%{opacity:1}}</style>
        <div id="Loader">
            <div id="SpinnerContainer">
                <div id="Spinner"></div>
                <p>Building the page...</p>
            </div>
        </div>`;
        document.body.appendChild(Loader);
    },

    LoadingFinished: () => {
        // Hide the loader
        const Loader = document.getElementById('Loader');
        // Loader.parentElement.removeChild(Loader);
        RK.WhenThePageIsResized();
        window.addEventListener('resize', RK.WhenThePageIsResized);
        document.getElementById('SearchIcon').addEventListener('click', RK.ChangeSearchState);
        document.getElementById('SearchBar').addEventListener('focus', function () {
            if(!document.getElementById('SearchIcon').hasAttribute('data-searchlaunched')) {
                RK.ChangeSearchState();
            }
        });
        document.getElementById('SearchBar').addEventListener('keyup', RK.LaunchSearch);
    },

    UpdateDescriptionLength: () => {	
        let Table = document.getElementById('ExerciseTable');
        if(Table) {
            var MainSection = document.getElementById('MainSection');
            let CompStylesMainSection = window.getComputedStyle(MainSection);

            var ParentArticle = Table.parentNode;
            let CompStylesParentArticle = window.getComputedStyle(ParentArticle);

            // Find the table, for now the table is not fixed, so column widths are set as needed. We'll use that to get the "natural" width of the first column so we can resize the second one accordingly.
            Table.style.tableLayout = "inherit";

            // Calculate the 2nd column width
            var FirstTD = Table.getElementsByTagName('td')[0];
            let CompStyles = window.getComputedStyle(FirstTD);
            var ColumnWidth = parseInt(MainSection.offsetWidth)
                            - parseInt(CompStylesMainSection.getPropertyValue('padding-left').split("px")[0])
                            - parseInt(CompStylesMainSection.getPropertyValue('padding-right').split("px")[0])
                            - parseInt(CompStylesParentArticle.getPropertyValue('padding-left').split("px")[0])
                            - parseInt(CompStylesParentArticle.getPropertyValue('padding-right').split("px")[0])
                            - parseInt(CompStyles.getPropertyValue('width').split("px")[0])
                            - parseInt(CompStyles.getPropertyValue('padding-left').split("px")[0])
                            - parseInt(CompStyles.getPropertyValue('padding-right').split("px")[0]);
            var AllRightTDs = Table.getElementsByClassName('EllipseableCell');

            // Now we can put the table layout as fixed so that we clearly give the width of cells.
            Table.style.tableLayout = "fixed";
            for(let I = AllRightTDs.length - 1; I >= 0; I--) {
                AllRightTDs[I].style.width = ColumnWidth + "px";
            }
        }
	},

    WhenThePageIsResized: () => {
        RK.ResizeSearchResultList();
        RK.UpdateDescriptionLength();
    },

    EnableDarkMode: () => {
        document.documentElement.style.setProperty('--StrongBackgroundColour', "rgb(40, 40, 40)");
        document.documentElement.style.setProperty('--BackgroundColour', "rgb(50, 50, 50)");
        document.documentElement.style.setProperty('--WeakBackgroundColour', "rgb(60, 60, 60)");
        document.documentElement.style.setProperty('--StrongColour', "white");
        document.documentElement.style.setProperty('--Colour', "rgb(245, 245, 245)");
        document.documentElement.style.setProperty('--WeakColour', "rgb(235, 235, 235)");
        document.documentElement.style.setProperty('--SpecialColour', "rgb(30, 171, 249)");
        document.documentElement.style.setProperty('--ShadowColour', "rgb(100, 100, 100)");
    },
    
    EnableLightMode: () => {
        document.documentElement.style.setProperty('--StrongBackgroundColour', "white");
        document.documentElement.style.setProperty('--BackgroundColour', "rgb(245, 245, 245)");
        document.documentElement.style.setProperty('--WeakBackgroundColour', "rgb(235, 235, 235)");
        document.documentElement.style.setProperty('--StrongColour', "rgb(40, 40, 40)");
        document.documentElement.style.setProperty('--Colour', "rgb(50, 50, 50)");
        document.documentElement.style.setProperty('--WeakColour', "rgb(60, 60, 60)");
        document.documentElement.style.setProperty('--SpecialColour', "rgb(30, 171, 249)");
        document.documentElement.style.setProperty('--ShadowColour', "rgb(200, 200, 200)");
    }, 
    
    ShowMessage: (Message, Class) => {
        let VerificationCode = document.getElementById('Output');
        let P = document.createElement('p');
        P.classList.add(Class);
        P.classList.add('Message');
        P.innerHTML = Message;
        VerificationCode.appendChild(P);
    },
    
    ShowError: (ErrorMessage) => {
        RK.ShowMessage(ErrorMessage, "ErrorMessage");
    },
    
    ShowWarning: (WarningMessage) => {
        RK.ShowMessage(WarningMessage, "ErrorMessage");
    },
    
    ShowGood: (GoodMessage) => {
        RK.ShowMessage(GoodMessage, "GoodMessage");
    }
};

if(window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches) {
    // Dark mode
    RK.EnableDarkMode();
}
else {
    // Light mode
    RK.EnableLightMode();
}

window.matchMedia('(prefers-color-scheme: dark)').addEventListener('change', e => {
    if(e.matches) {
        // Dark theme
        RK.EnableDarkMode();
    }
    else {
        // Light theme
        RK.EnableLightMode();
    }
});