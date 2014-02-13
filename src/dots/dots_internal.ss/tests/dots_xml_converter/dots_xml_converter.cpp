/******************************************************************************
*
* Copyright Saab AB, 2004-2013 (http://safir.sourceforge.net)
*
* Created by: Joel Ottosson / joot
*
*******************************************************************************/
#include <iostream>
#include <fstream>
#include <map>
#include <set>
#include <algorithm>
#include <stack>
#include <boost/lexical_cast.hpp>
#include <boost/property_tree/xml_parser.hpp>
#include <Safir/Dob/Typesystem/ToolSupport/TypeParser.h>
#include <Safir/Dob/Typesystem/ToolSupport/BlobLayout.h>
#include <Safir/Dob/Typesystem/Internal/Id.h>
#include <Safir/Dob/Typesystem/ToolSupport/Serialization.h>

class Convert
{
public:
    Convert(const std::string& douDir, const std::string& srcDir, const std::string& outDir)
        :m_douDir(douDir)
        ,m_srcDir(srcDir)
        ,m_outDir(outDir)
        ,m_rep()
    {
        try
        {
            m_rep=Safir::Dob::Typesystem::ToolSupport::ParseTypeDefinitions(m_douDir);
        }
        catch (const Safir::Dob::Typesystem::ToolSupport::ParseError& err)
        {
            std::cout<<err.what()<<std::endl;
            exit(1);
        }
    }

    void Run()
    {
        TraverseDir(m_srcDir);
    }

private:
    boost::filesystem::path m_douDir;
    boost::filesystem::path m_srcDir;
    boost::filesystem::path m_outDir;
    boost::shared_ptr<const Safir::Dob::Typesystem::ToolSupport::TypeRepository> m_rep;

    void FindTags(const std::string& str, std::vector< std::pair<size_t, size_t> >& objects)
    {
        std::map<size_t, bool> tags;

        size_t next=0;
        for(;;)
        {
            next=str.find("<object", next);
            if (next==std::string::npos)
            {
                break;
            }
            tags[next]=true;
            ++next;
        }

        next=0;
        for(;;)
        {
            next=str.find("</object", next);
            if (next==std::string::npos)
            {
                break;
            }
            tags[next+9]=false; //position after end
            ++next;
        }

        if (tags.empty())
        {
            return;
        }

        std::pair<size_t, size_t> current=std::make_pair<size_t, size_t>(0, 0);
        int stack=0;
        for (std::map<size_t, bool>::const_iterator it=tags.begin(); it!=tags.end(); ++it)
        {
            if (stack==0)
            {
                if (!it->second)
                {
                    std::cout<<"Unexpected end element: at "<<it->first<<std::endl;
                    exit(1);
                }

                current.first=it->first;
            }

            if (it->second)
            {
                ++stack;
            }
            else
            {
                --stack;
            }

            if (stack==0)
            {
                current.second=it->first;
                objects.push_back(current);
                current.first=0;
                current.second=0;
            }
        }
    }

    void TraverseDir(const boost::filesystem::path& root)
    {
        std::cout<<"+"<<root.string()<<std::endl;
        boost::filesystem::directory_iterator it(root), end;
        for (; it!=end; ++it)
        {
            if (boost::filesystem::is_directory(it->path()))
            {
                TraverseDir(it->path());
            }
            else
            {
                FixFile(it->path());
            }
        }
    }

    void FixFile(const boost::filesystem::path& file)
    {
        std::cout<<"   -"<<file.filename()<<std::endl;

        std::string raw;
        {
            std::ostringstream tmp;
            std::ifstream is(file.string().c_str());
            tmp<<is.rdbuf();
            is.close();
            raw=tmp.str();
        }

        std::string subtree=file.string().substr(m_srcDir.string().size());
        boost::filesystem::path outFile=m_outDir/subtree;
        boost::filesystem::create_directories(outFile.parent_path());
        std::cout<<"  outfile: "<<outFile.string()<<std::endl;

        std::ofstream of(outFile.string().c_str(), std::ios::out);

        if (!of.is_open())
        {
            std::cout<<"Failed to open file "<<outFile.string()<<std::endl;
            exit(1);
        }

        std::vector< std::pair<size_t, size_t> > objects;
        FindTags(raw, objects);

        size_t index=0;
        for (size_t i=0; i<objects.size(); ++i)
        {
            while (index<objects[i].first)
            {
                of<<raw[index++];
            }


            std::string xmlSrc=raw.substr(objects[i].first, objects[i].second-objects[i].first);
            try
            {
                std::vector<char> blob;
                Safir::Dob::Typesystem::ToolSupport::XmlToBinary(m_rep.get(), xmlSrc.c_str(), blob);

                std::ostringstream xmlDest;
                Safir::Dob::Typesystem::ToolSupport::BinaryToXml(m_rep.get(), &blob[0], xmlDest);
                of<<xmlDest.str();

//                std::string xml=xmlDest.str();
//                if (boost::starts_with(xml, "<?xml")) //remove <?xml version="1.0" encoding="utf-8"?>
//                {
//                    of<<xml.substr(xml.find("?>")+2);

//                }
//                else
//                {
//                    of<<xmlDest.str();
//                }
            }
            catch (const Safir::Dob::Typesystem::ToolSupport::ParseError& err)
            {
                std::cout<<err.what()<<" while parsing: "<<std::endl<<xmlSrc<<std::endl;
                exit(1);
            }

            index=objects[i].second;
        }

        while (index<raw.size())
        {
            of<<raw[index++];
        }

        of.close();
    }
};

int main(int argc, char* argv[])
{    
    if (argc<4)
    {
        std::cout<<"usage: dots_xm_converter douDir srcDir outDir"<<std::endl;
    }
    //-----------------------------------------------------------
    //Parse dou files for this test and create a type repository
    //-----------------------------------------------------------
    //std::string douDir="/home/joot/safir/runtime/data/text/dots/classes";
    //std::string srcDir="/home/joot/dose_test_output/unconverted";
    //std::string outDir="/home/joot/dose_test_output/converted";
    std::string douDir=argv[1];
    std::string srcDir=argv[2];
    std::string outDir=argv[3];

    Convert conv(douDir, srcDir, outDir);

    conv.Run();

    return 0;
}
