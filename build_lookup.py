from genericpath import isfile
import os, sys
from glob import glob
import json

# Open the lookup file
LookupFile = open("lookup.json", "w")
LookupFileString = "[\n"
Technologies = ["MPI", "OpenMP"]
FirstTechnologyEntry = True
for Technology in Technologies:
    if(not FirstTechnologyEntry):
        LookupFileString += ",\n    "
    else:
        FirstTechnologyEntry = False
    TechnologyCategories = dict()
    TechnologyCategories["Type"] = "DocumentationHomepage"
    TechnologyCategories["Technology"] = Technology
    TechnologyCategories["Categories"] = dict()
    TechnologyDocsDirectory = Technology.lower() + "/docs"
    LookupFileString += '    {\n'
    LookupFileString += '        "Technology":"' + Technology + '",\n'
    LookupFileString += '        "PathRoot":"/' + TechnologyDocsDirectory + '",\n'
    LookupFileString += '        "Entries":[\n'
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
                        LookupFileString += ",\n"
                    else:
                        FirstDocumentationEntry = False
                    LookupFileString += '            {\n'
                    LookupFileString += '                "Name":"' + JsonEntry["Name"] + '",\n'
                    LookupFileString += '                "DirectoryName":"' + DocumentationEntryDirectory + '"\n'
                    LookupFileString += '            }'
                    FirstCategory = True
                    for Category in JsonEntry["Categories"]:
                        if Category not in TechnologyCategories["Categories"]:
                            TechnologyCategories["Categories"][Category] = []
                        TechnologyCategories["Categories"][Category].append(JsonEntry["Name"])
                    # Close the file
                    DocumentationEntryJsonFile.close()
                except ValueError as e:
                    print('The json file "' + DocumentationEntryJsonFilePath + '" is invalid: ' + str(e))
                    quit()
            else:
                print(DocumentationEntryJsonFilePath + " does not exist")
    LookupFileString += '\n        ]\n'
    LookupFileString += '    }'
    TechnologyLookupFile = open(TechnologyDocsDirectory + "/data.json", "w")
    TechnologyLookupFile.write(json.dumps(TechnologyCategories, sort_keys=True))
    TechnologyLookupFile.close()

LookupFileString += "\n]"
LookupFile.write(LookupFileString)
LookupFile.close()

# Build exercise homepages
for Technology in Technologies:
    HomepageFilePath = Technology.lower() + "/exercises/data.json"
    HomepageFile = open(HomepageFilePath, "w")
    HomepageFileContent = "{\n"
    HomepageFileContent += "    \"Type\":\"ExerciseHomepage\",\n"
    HomepageFileContent += "    \"Technology\":\"" + Technology + "\",\n"
    HomepageFileContent += "    \"Exercises\":["
    ExerciseIndex = 1
    ExerciseFilePath = Technology.lower() + "/exercises/exercise_" + str(ExerciseIndex) + "/data.json"
    FirstExercise = True
    while(os.path.exists(ExerciseFilePath)):
        ExerciseFile = open(ExerciseFilePath, "r")
        if(FirstExercise):
            FirstExercise = False
        else:
            HomepageFileContent += ","
        HomepageFileContent += "\n        {\n"
        JSONObject = json.load(ExerciseFile)
        HomepageFileContent += "            \"Name\":\"" + JSONObject["Title"] + "\",\n"
        HomepageFileContent += "            \"Description\":\"" + JSONObject["Introduction"] + "\",\n"
        HomepageFileContent += "            \"DirectoryName\":\"" + JSONObject["DirectoryName"] + "\",\n"
        HomepageFileContent += "            \"Difficulty\":" + str(JSONObject["Difficulty"]) + "\n"
        HomepageFileContent += "        }"
        ExerciseFile.close()
        ExerciseIndex = ExerciseIndex + 1
        ExerciseFilePath = Technology.lower() + "/exercises/exercise_" + str(ExerciseIndex) + "/data.json"
    HomepageFileContent += "\n"
    HomepageFileContent += "    ]\n"
    HomepageFileContent += "}"
    HomepageFile.write(HomepageFileContent)
    HomepageFile.close()

# Build sitemap
SitemapFile = open("sitemap/sitemap.xml", "w")
SitemapFileString = "<urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">\n"
AllIndexHtmlFiles = [y for x in os.walk(".") for y in glob(os.path.join(x[0], 'index.html'))]
for AllIndexHtmlFile in AllIndexHtmlFiles:
    SitemapFileString += "    <url>\n        <loc>" + AllIndexHtmlFile[1:] + "</loc>\n    </url>\n"
SitemapFileString += "</urlset>"
SitemapFile.write(SitemapFileString)
SitemapFile.close()

SitemapPageFile = open("sitemap/data.json", "w")
SitemapPageFileString = "{\n"
SitemapPageFileString += "    \"Type\":\"Text\",\n"
SitemapPageFileString += "    \"DirectoryName\":\"sitemap\",\n"
SitemapPageFileString += "    \"Name\":\"sitemap\",\n"
SitemapPageFileString += "    \"Title\":\"Sitemap\",\n"
SitemapPageFileString += "    \"Sections\":[\n"
SitemapPageFileString += "        {\n"
SitemapPageFileString += "            \"Title\":\"List of all pages\",\n"
SitemapPageFileString += "            \"Content\":\"<ul>"
for AllIndexHtmlFile in AllIndexHtmlFiles:
    SitemapPageFileString += "<li><a href=\\\"" + AllIndexHtmlFile[1:] + "\\\">" + AllIndexHtmlFile[1:] + "</a></li>"
SitemapPageFileString += "</ul>\"\n"
SitemapPageFileString += "        }\n"
SitemapPageFileString += "    ]\n"
SitemapPageFileString += "}"
SitemapPageFile.write(SitemapPageFileString)
SitemapPageFile.close()