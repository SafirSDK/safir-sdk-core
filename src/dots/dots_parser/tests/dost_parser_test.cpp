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
#ifdef _MSC_VER
    argc; argv;
#endif
   
    boost::timer timer; 
    double elapsed = 0;
    
    boost::filesystem::path douDir = boost::filesystem::path("C:/dev/safir_sdk_core/src/dots/dots_parser/tests/dou_test_files");
    //boost::filesystem::path douDir = boost::filesystem::path("/home/joel/dev/safir_open/src/dots/dots_parser/tests/dou_test_files");    

    Safir::Dob::Typesystem::Internal::TypeDefinitionParser tp;

    try
    {
        tp.Parse(douDir);
        elapsed=timer.elapsed();
        //tp.Dump(tp.GetResult());

        std::cout<<"Parsed OK"<<std::endl;
    }
    catch (Safir::Dob::Typesystem::Internal::ParseError err)
    {
        elapsed=timer.elapsed();

        std::cout<<"********** Parse Error **********************************************"<<std::endl;
        std::cout<<"* Label: "<<err.Label()<<std::endl;
        std::cout<<"* Descr: "<<err.Description()<<std::endl;
        std::cout<<"* File:  "<<err.File()<<std::endl;
        std::cout<<"*********************************************************************"<<std::endl;
        
        std::cerr<<err.File()<<":1:1: error: hej"<<std::endl;
    }
    catch (std::exception err)
    {
        std::cout<<"Unknown exception: "<<err.what()<<std::endl;
    }

    std::cout<<"Time elapsed: "<<elapsed<<std::endl;
    return 0;
}
