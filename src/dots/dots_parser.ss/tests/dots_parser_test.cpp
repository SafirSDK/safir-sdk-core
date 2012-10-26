/******************************************************************************
*
* Copyright Saab AB, 2004-2012 (http://www.safirsdk.com)
*
* Created by: Joel Ottosson / joot
*
*******************************************************************************/
#include <iostream>
#include <boost/timer.hpp>
#include <Safir/Dob/Typesystem/Internal/TypeDefinitionParser.h>

int main(int argc, char* argv[])
{
    boost::filesystem::path douDir;// = boost::filesystem::path("/home/joel/dev/safir_open/src/dots/dots_parser/tests/dou_test_files");
    if (argc>1)
    {
        douDir = boost::filesystem::path(argv[1]);
    }
    else
    {
        //douDir = boost::filesystem::path("C:/dev/safir_sdk_core/src/dots/dots_parser/tests/dou_test_files");
        douDir = boost::filesystem::path("/home/joel/dev/safir_open/src/dots/dots_parser.ss/tests/dou_test_files");
    }

    std::cout<<"DouDir: "<<douDir.string()<<std::endl;
   
    boost::timer timer; 
    double elapsed = 0;
    
    boost::shared_ptr<Safir::Dob::Typesystem::Internal::TypeDefinitionParser> parser;

    try
    {
        parser.reset(new Safir::Dob::Typesystem::Internal::TypeDefinitionParser(douDir));
        elapsed=timer.elapsed();
        //std::cout<<Safir::Dob::Typesystem::Internal::TypeDefinitionParser::ToString(parser->GetRepository())<<std::endl;
        //std::cout<<Safir::Dob::Typesystem::Internal::TypeDefinitionParser::ToString(parser->GetRawResult())<<std::endl;

        //std::cout<<"Parsed OK"<<std::endl;
    }
    catch (Safir::Dob::Typesystem::Internal::ParseError err)
    {
        elapsed=timer.elapsed();

        std::cout<<"********** Parse Error **********************************************"<<std::endl;
        std::cout<<"* Label: "<<err.Label()<<std::endl;
        std::cout<<"* Descr: "<<err.Description()<<std::endl;
        std::cout<<"* File:  "<<err.File()<<std::endl;
        std::cout<<"*********************************************************************"<<std::endl;
        
        //std::cerr<<err.File()<<":1:1: error: hej"<<std::endl;
    }
    catch (std::exception err)
    {
        std::cout<<"Unknown exception: "<<err.what()<<std::endl;
        return 1; //Test failed
    }

    std::cout<<"Time elapsed: "<<elapsed<<std::endl;
    return 0;
}
