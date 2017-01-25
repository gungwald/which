#include <iostream>
#include <cstdlib>
#include <string>
#include <vector>

using namespace std;

vector<string> split(string &splittethMe, const char thySplittor)
{
    string::size_type beginIndex = 0;
    string::size_type splittorIndex;
    string part;
    string::size_type partLength;
    string splittor(1, thySplittor);
    vector<string> splitten;
    
    // Get all but the last part
    while ((splittorIndex = splittethMe.find(splittor, beginIndex)) != string::npos) {
        partLength = splittorIndex - beginIndex;
        part = splittethMe.substr(beginIndex, partLength);
        splitten.push_back(part);
        beginIndex = splittorIndex + 1;
    }
    // Get the last part, if there is one
    if (beginIndex < splittethMe.length()) {
        partLength = splittethMe.length() - beginIndex;
        part = splittethMe.substr(beginIndex, partLength);
        splitten.push_back(part);
    }
    return splitten;
}

int main(int argc, char *argv[])
{
    char *cpathExt = getenv("PATHEXT");
    if (cpathExt != NULL) {
	    string pathExt(cpathExt);
	    vector<string> extensions = split(pathExt, ';');
    }

    char *cpath = getenv("PATH");
    if (cpath == NULL) {
        cerr << "PATH not found" << endl;
    }
    string path(cpath);
    vector<string> splittenPath = split(path, ':');
    for (int i = 0; i < splittenPath.size(); i++) {
        cout << splittenPath[i] << endl;
    }
    // system("PAUSE");	
    return 0;
}
