from genericpath import isfile
import os, sys
import json

# Open the lookup file
LookupFile = open("lookup.json", "w")
LookupFileString = "["
Technologies = ["MPI"]
FirstTechnologyEntry = True
for Technology in Technologies:
    if(not FirstTechnologyEntry):
        LookupFileString += ","
    else:
        FirstTechnologyEntry = False
    TechnologyCategories = dict()
    TechnologyCategories["Type"] = "DocumentationHomepage"
    TechnologyCategories["Technology"] = Technology
    TechnologyCategories["Categories"] = dict()
    TechnologyDocsDirectory = Technology.lower() + "/docs"
    LookupFileString += '{"Technology":"' + Technology + '","PathRoot":"/' + TechnologyDocsDirectory + '","Entries":['
    DocumentationEntryDirectories = sorted(os.listdir(TechnologyDocsDirectory))
    FirstDocumentationEntry = True
    # This would print all the files and directories
    for DocumentationEntryDirectory in DocumentationEntryDirectories:
        DocumentationEntryPath = TechnologyDocsDirectory + '/' + DocumentationEntryDirectory
        if(os.path.isdir(DocumentationEntryPath)):
            DocumentationEntryJsonFilePath = DocumentationEntryPath + "/data.json"
            if(os.path.isfile(DocumentationEntryJsonFilePath)):
                print('Parsing "' + DocumentationEntryJsonFilePath + '"')
                # Open it
                DocumentationEntryJsonFile = open(DocumentationEntryJsonFilePath, "r")
                try:
                    JsonEntry = json.load(DocumentationEntryJsonFile)
                    if(not FirstDocumentationEntry):
                        LookupFileString += ","
                    else:
                        FirstDocumentationEntry = False
                    LookupFileString += '{"Name":"' + JsonEntry["Name"] + '","DirectoryName":"' + DocumentationEntryDirectory + '"}'
                    FirstCategory = True
                    for Category in JsonEntry["Categories"]:
                        if Category not in TechnologyCategories["Categories"]:
                            TechnologyCategories["Categories"][Category] = []
                        TechnologyCategories["Categories"][Category].append(JsonEntry["Name"])
                    # Close the file
                    DocumentationEntryJsonFile.close()
                except ValueError as e:
                    print('The json file "' + DocumentationEntryJsonFilePath + '" is invalid.')
            else:
                print(DocumentationEntryJsonFilePath + " does not exist")
    LookupFileString += ']}'
    TechnologyLookupFile = open(TechnologyDocsDirectory + "/data.json", "w")
    TechnologyLookupFile.write(json.dumps(TechnologyCategories, sort_keys=True))
    TechnologyLookupFile.close()

LookupFileString += "]"
LookupFile.write(LookupFileString)
LookupFile.close()