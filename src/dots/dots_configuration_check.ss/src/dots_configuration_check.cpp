/******************************************************************************
*
* Copyright Saab AB, 2005-2013 (http://safir.sourceforge.net)
* 
* Created by: Lars Hagström / stlrha
*
*******************************************************************************
*
* This file is part of Safir SDK Core.
*
* Safir SDK Core is free software: you can redistribute it and/or modify
* it under the terms of version 3 of the GNU General Public License as
* published by the Free Software Foundation.
*
* Safir SDK Core is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with Safir SDK Core.  If not, see <http://www.gnu.org/licenses/>.
*
******************************************************************************/
#include <iostream>
#include <boost/program_options.hpp>
#include <Safir/Dob/Typesystem/Internal/Kernel.h>


#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable : 4702)
#endif

class CmdLine
{
public:
    CmdLine(int argc, char * argv[])
        :summary(false)
        ,details(false)
        ,typeName()
        ,paths()
    {
        boost::program_options::options_description desc("Command line options");
        desc.add_options()
                ("help,h", "Produce help message")
                ("summary,s", "Output a brief summary of the type system'")
                ("details,d", "Output a full information about the entire type system")
                ("type,t", boost::program_options::value<std::string>(), "Output info about a specific type")
                ("path", boost::program_options::value< std::vector<std::string> >()->multitoken(), "Parse specified path(s) into a local memory type repostionry.");

        boost::program_options::variables_map vm;
        boost::program_options::store(boost::program_options::parse_command_line(argc, argv, desc), vm);
        boost::program_options::notify(vm);
        if (vm.count("help"))
        {
            std::cout<<desc<<std::endl;
            return;
        }

        summary=vm.count("summary");
        details=vm.count("details");
        if (vm.count("type"))
        {
            typeName=vm["type"].as<std::string>();
        }

        if (vm.count("path"))
        {
            paths=vm["path"].as< std::vector<std::string> >();
        }
    }

    bool summary;
    bool details;
    std::string typeName;
    std::vector<std::string> paths;
};

void SimpleCheck();
void ShowSummary();
void ShowDetails();
void ShowType(const char* name);
void ShowClass(DotsC_TypeId tid);
void ShowEnum(DotsC_TypeId tid);
void ShowProperty(DotsC_TypeId tid);
void ShowException(DotsC_TypeId tid);
std::string ParameterValue(DotsC_TypeId tid, DotsC_ParameterIndex par);

int main(int argc, char* argv[])
{
    CmdLine cmd(argc, argv);

    try
    {
        if (!cmd.typeName.empty())
        {
            ShowType(cmd.typeName.c_str());
        }
        else if (cmd.summary)
        {
            ShowSummary();
        }
        else if (cmd.details)
        {
            ShowDetails();
        }
        else
        {
            SimpleCheck();
        }
    }
    catch (const std::exception & exc)
    {
        std::wcout << "Failed with exception description: " << exc.what() << std::endl;
        exit(1);
    }
    catch (...)
    {
        std::wcout << "Failed with ... exception." << std::endl;
        exit(1);
    }

    return 0;
}

void SimpleCheck()
{
    std::wcout<<"Checking configuration..."<<std::endl;
    DotsC_NumberOfTypeIds();
    std::wcout<<"Success!"<<std::endl;
}

void ShowSummary()
{
    if (DotsC_TypeRepositoryLoadedByThisProcess())
    {
        std::wcout<<L"Type system created by this process."<<std::endl;
    }
    else
    {
        std::wcout<<L"Type system loaded, was already created."<<std::endl;

    }
    std::wcout<<L"Number of classes:     "<<DotsC_NumberOfClasses()<<std::endl;
    std::wcout<<L"Number of enums:       "<<DotsC_NumberOfEnumerations()<<std::endl;
    std::wcout<<L"Number of properties:  "<<DotsC_NumberOfProperties()<<std::endl;
    std::wcout<<L"Number of exceptions:  "<<DotsC_NumberOfExceptions()<<std::endl;
    std::wcout<<L"Total number of types: "<<DotsC_NumberOfTypeIds()<<std::endl;
}

void ShowDetails()
{
    DotsC_TypeId tid[3000];
    int size;
    DotsC_GetAllTypeIds(tid, 3000, size);
    for (int i=0; i<size; ++i)
    {
        const char* n=DotsC_GetTypeName(tid[i]);
        ShowType(n);
        std::wcout<<std::endl;
    }

    std::wcout<<std::endl<<"----------"<<std::endl;
    ShowSummary();
}

void ShowType(const char* name)
{
    DotsC_TypeId tid=DotsC_TypeIdFromName(name);
    if (DotsC_IsClass(tid))
    {
        ShowClass(tid);
    }
    else if (DotsC_IsEnumeration(tid))
    {
        ShowEnum(tid);
    }
    else if (DotsC_IsProperty(tid))
    {
        ShowProperty(tid);
    }
    else if (DotsC_IsException(tid))
    {
        ShowException(tid);
    }
    else
    {
        std::wcout<<L"The type '"<<name<<"' doesn't exist!"<<std::endl;
    }
}

void ShowClass(DotsC_TypeId tid)
{
    std::wcout<<"Class: "<<DotsC_GetTypeName(tid)<<" ["<<tid<<"]"<<std::endl;

    int numMem=DotsC_GetNumberOfMembers(tid);
    if (numMem>0)
    {
        std::wcout<<"  Members:"<<std::endl;
    }
    for (int i=0; i<numMem; ++i)
    {
        const char* name;
        DotsC_MemberType mt;
        DotsC_TypeId complexTid;
        int strLen;
        bool isArray;
        int arrSize;
        DotsC_GetMemberInfo(tid, i, mt, name, complexTid, strLen, isArray, arrSize);
        if (mt!=ObjectMemberType && mt!=EnumerationMemberType)
        {
            std::wcout<<"    "<<i<<". "<<DotsC_GetMemberTypeName(tid, i)<<" "<<name<<std::endl;
        }
        else
        {
            std::wcout<<"    "<<i<<". "<<DotsC_GetTypeName(complexTid)<<" "<<name<<std::endl;
        }
    }

    int numParams=DotsC_GetNumberOfParameters(tid);
    if (numParams>0)
    {
        std::wcout<<"  Parameters:"<<std::endl;
    }
    for (int i=0; i<numParams; ++i)
    {
        const char* n=DotsC_GetParameterName(tid, i);
        const char* t=DotsC_GetParameterTypeName(tid, i);
        std::wcout<<"    "<<i<<". "<<t<<" "<<n<<std::endl;
        std::wcout<<ParameterValue(tid, i).c_str()<<std::endl;
    }
}

void ShowEnum(DotsC_TypeId tid)
{
    std::wcout<<"Enum: "<<DotsC_GetTypeName(tid)<<" ["<<tid<<"]"<<std::endl;

    int numVal=DotsC_GetNumberOfEnumerationValues(tid);
    for (int i=0; i<numVal; ++i)
    {
        const char* n=DotsC_GetEnumerationValueName(tid, i);
        std::wcout<<"  Value "<<i<<": "<<n<<std::endl;
    }
}

void ShowProperty(DotsC_TypeId tid)
{
    std::wcout<<"Property: "<<DotsC_GetTypeName(tid)<<" ["<<tid<<"]"<<std::endl;
}

void ShowException(DotsC_TypeId tid)
{
    std::wcout<<"Exception: "<<DotsC_GetTypeName(tid)<<" ["<<tid<<"]"<<std::endl;
}

std::string ParameterValue(DotsC_TypeId tid, DotsC_ParameterIndex par)
{
    std::ostringstream os;
    DotsC_MemberType mt=DotsC_GetParameterType(tid, par);
    int arrSize=DotsC_GetParameterArraySize(tid, par);

    //Values
    switch(mt)
    {
    case BooleanMemberType:
    {
        for (int i=0; i<arrSize; ++i)
        {
            bool val=false;
            DotsC_GetBooleanParameter(tid, par, i, val);
            os<<"            value["<<i<<"]="<<std::boolalpha<<val<<std::dec<<std::endl;
        }
    }
        break;

    case Int32MemberType:
    {
        for (int i=0; i<arrSize; ++i)
        {
            DotsC_Int32 val=0;
            DotsC_GetInt32Parameter(tid, par, i, val);
            os<<"            value["<<i<<"]="<<val<<std::endl;
        }
    }
        break;
    case Int64MemberType:
    {
        for (int i=0; i<arrSize; ++i)
        {
            DotsC_Int64 val=0;
            DotsC_GetInt64Parameter(tid, par, i, val);
            os<<"            value["<<i<<"]="<<val<<std::endl;
        }
    }
        break;

    case EntityIdMemberType:
    {
        for (int i=0; i<arrSize; ++i)
        {
            DotsC_EntityId val;
            const char* inst;
            DotsC_GetEntityIdParameter(tid, par, i, val, inst);
            os<<"            value["<<i<<"]={"<<DotsC_GetTypeName(val.typeId)<<", ";
            if (inst!=NULL)
                os<<inst<<"}"<<std::endl;
            else
                os<<val.instanceId<<"}"<<std::endl;
        }
    }
        break;
    case TypeIdMemberType:
    {
        for (int i=0; i<arrSize; ++i)
        {
            DotsC_TypeId val=0;
            DotsC_GetInt64Parameter(tid, par, i, val);
            os<<"            value["<<i<<"]="<<DotsC_GetTypeName(val)<<std::endl;
        }
    }
        break;
    case InstanceIdMemberType:
    case ChannelIdMemberType:
    case HandlerIdMemberType:
    {
        for (int i=0; i<arrSize; ++i)
        {
            DotsC_Int64 hash=0;
            const char* str=NULL;
            DotsC_GetHashedIdParameter(tid, par, i, hash, str);
            if (str!=NULL)
                os<<"            value["<<i<<"]="<<str<<std::endl;
            else
                os<<"            value["<<i<<"]="<<hash<<std::endl;
        }
    }
        break;

    case StringMemberType:
    {
        for (int i=0; i<arrSize; ++i)
        {
            const char* str;
            DotsC_GetStringParameter(tid, par, i, str);
            os<<"            value["<<i<<"]="<<str<<std::endl;
        }
    }
        break;

    case ObjectMemberType:
    {
        for (int i=0; i<arrSize; ++i)
        {
            const char* val;
            DotsC_GetObjectParameter(tid, par, i, val);
            std::vector<char> json;
            json.resize(1000);
            DotsC_Int32 resultSize;
            DotsC_BlobToJson(&json[0], val, json.size(), resultSize);
            os<<"            value["<<i<<"]="<<&json[0]<<std::endl;
        }
    }
        break;

    case EnumerationMemberType:
    {
        for (int i=0; i<arrSize; ++i)
        {
            DotsC_EnumerationValue val;
            DotsC_GetEnumerationParameter(tid, par, i, val);
            DotsC_TypeId enumTid=DotsC_TypeIdFromName(DotsC_GetParameterTypeName(tid, par));
            os<<"            value["<<i<<"]="<<DotsC_GetEnumerationValueName(enumTid, val)<<std::endl;
        }
    }
        break;

    case BinaryMemberType:
    {
        for (int i=0; i<arrSize; ++i)
        {
            const char* val;
            DotsC_Int32 size, resultSize;
            DotsC_GetBinaryParameter(tid, par, i, val, size);
            std::vector<char> b64;
            b64.resize(1000);
            DotsC_BinaryToBase64(&b64[0], b64.size(), val, size, resultSize);
            std::string b64str(b64.begin(), b64.begin()+resultSize);
            os<<"            value["<<i<<"]="<<b64str<<std::endl;
        }

    }
        break;

    case Float32MemberType:
    case Ampere32MemberType:
    case CubicMeter32MemberType:
    case Hertz32MemberType:
    case Joule32MemberType:
    case Kelvin32MemberType:
    case Kilogram32MemberType:
    case Meter32MemberType:
    case MeterPerSecond32MemberType:
    case MeterPerSecondSquared32MemberType:
    case Newton32MemberType:
    case Pascal32MemberType:
    case Radian32MemberType:
    case RadianPerSecond32MemberType:
    case RadianPerSecondSquared32MemberType:
    case Second32MemberType:
    case SquareMeter32MemberType:
    case Steradian32MemberType:
    case Volt32MemberType:
    case Watt32MemberType:
    {
        for (int i=0; i<arrSize; ++i)
        {
            DotsC_Float32 val=0;
            DotsC_GetFloat32Parameter(tid, par, i, val);
            os<<"            value["<<i<<"]="<<val<<std::endl;
        }
    }
        break;

    case Float64MemberType:
    case Ampere64MemberType:
    case CubicMeter64MemberType:
    case Hertz64MemberType:
    case Joule64MemberType:
    case Kelvin64MemberType:
    case Kilogram64MemberType:
    case Meter64MemberType:
    case MeterPerSecond64MemberType:
    case MeterPerSecondSquared64MemberType:
    case Newton64MemberType:
    case Pascal64MemberType:
    case Radian64MemberType:
    case RadianPerSecond64MemberType:
    case RadianPerSecondSquared64MemberType:
    case Second64MemberType:
    case SquareMeter64MemberType:
    case Steradian64MemberType:
    case Volt64MemberType:
    case Watt64MemberType:
    {
        for (int i=0; i<arrSize; ++i)
        {
            DotsC_Float64 val=0;
            DotsC_GetFloat64Parameter(tid, par, i, val);
            os<<"            value["<<i<<"]="<<val<<std::endl;
        }
    }
        break;
    }

    return os.str();
}


////===================================================================================================
////forward declarations
//class ClassDescriptionKernel;

//class MemberDescriptionKernel
//{
//public:
//    MemberDescriptionKernel(DotsC_TypeId tid, DotsC_MemberIndex member)
//        :m_tid(tid)
//        ,m_member(member)
//    {
//        DotsC_GetMemberInfo(m_tid, m_member, m_memberType, m_name, m_complexTid, m_strLen, m_isArray, m_arraySize);
//    }

//    const char* Summary() const {return NULL;}
//    DotsC_TypeId GetTypeId() const {return m_complexTid;}
//    const char* GetName() const {return m_name;}
//    DotsC_MemberType GetMemberType() const {return m_memberType;}
//    const bool IsArray() const {return m_isArray;}
//    int GetArraySize() const {return m_arraySize;}
//    int GetMaxLength() const {return m_strLen;}

//private:
//    DotsC_TypeId m_tid;
//    DotsC_MemberIndex m_member;

//    DotsC_MemberType m_memberType;
//    const char* m_name;
//    DotsC_TypeId m_complexTid;
//    bool m_isArray;
//    int m_strLen;
//    int m_arraySize;
//};

//class PropertyDescriptionKernel
//{
//public:
//    PropertyDescriptionKernel(DotsC_TypeId tid)
//        :m_tid(tid)
//    {
//        for (int i=0; i<DotsC_GetNumberOfMembers(m_tid); ++i)
//        {
//            m_members.push_back(MemberDescriptionKernel(m_tid, i));
//        }
//    }

//    const char* FileName() const {return DotsC_GetDouFilePath(m_tid);}
//    const char* Summary() const {return NULL;}
//    DotsC_TypeId GetTypeId() const {return m_tid;}
//    const char* GetName() const {return DotsC_GetTypeName(m_tid);}
//    int GetNumberOfMembers() const {return DotsC_GetNumberOfMembers(m_tid);}
//    DotsC_MemberIndex GetMemberIndex(const std::string& memberName) const {return DotsC_GetMemberId(m_tid, memberName.c_str());}
//    const MemberDescriptionKernel* GetMember(DotsC_MemberIndex index) const {return &m_members[index];}
//private:
//    DotsC_TypeId m_tid;
//    std::vector<MemberDescriptionKernel> m_members;
//};

//class ExceptionDescriptionKernel
//{
//public:
//    ExceptionDescriptionKernel(DotsC_TypeId tid)
//        :m_tid(tid)
//    {

//    }
//    const char* FileName() const {return DotsC_GetDouFilePath(m_tid);}
//    const char* Summary() const {return NULL;}
//    DotsC_TypeId GetTypeId() const {return m_tid;}
//    const char* GetName() const {return DotsC_GetTypeName(m_tid);}
//    const ExceptionDescriptionKernel* GetBaseClass() const {return NULL;}
//private:
//    DotsC_TypeId m_tid;
//};

//class ParameterDescriptionKernel
//{
//public:
//    ParameterDescriptionKernel(DotsC_TypeId tid, DotsC_ParameterIndex param)
//        :m_tid(tid)
//        ,m_param(param)
//    {
//    }

//    const char* Summary() const {return NULL;}
//    const char* GetName() const {return DotsC_GetParameterName(m_tid, m_param);}
//    DotsC_MemberType GetMemberType() const {return DotsC_GetParameterType(m_tid, m_param);}
//    DotsC_TypeId GetTypeId() const
//    {
//        const char* typeName=DotsC_GetParameterTypeName(m_tid, m_param);
//        return DotsC_TypeIdFromName(typeName);
//    }
//    bool IsArray() const {return GetArraySize()>0;}
//    int GetArraySize() const {return DotsC_GetParameterArraySize(m_tid, m_param);}
//    bool IsHidden() const {std::string name(GetName()); return name.find('@')!=name.npos;}

//    //Get parameter values - depending on actual type of the parameter
//    //For entityId use GetInt64Value for typeId and GetHashedValue for instanceId
//    boost::int32_t GetInt32Value(int index) const {DotsC_Int32 val; DotsC_GetInt32Parameter(m_tid, m_param, index, val); return val;}
//    boost::int64_t GetInt64Value(int index) const {DotsC_Int64 val; DotsC_GetInt64Parameter(m_tid, m_param, index, val); return val;}
//    float GetFloat32Value(int index) const {DotsC_Float32 val; DotsC_GetFloat32Parameter(m_tid, m_param, index, val); return val;}
//    double GetFloat64Value(int index) const {DotsC_Float64 val; DotsC_GetFloat64Parameter(m_tid, m_param, index, val); return val;}
//    bool GetBoolValue(int index) const {bool val; DotsC_GetBooleanParameter(m_tid, m_param, index, val); return val;}
//    const char* GetStringValue(int index) const {const char* val;  DotsC_GetStringParameter(m_tid, m_param, index, val); return val;}
//    std::pair<const char*, size_t> GetObjectValue(int index) const {const char* val; DotsC_GetObjectParameter(m_tid, m_param, index, val); return std::make_pair(val, static_cast<size_t>(DotsC_GetSize(val)));}
//    std::pair<const char*, size_t> GetBinaryValue(int index) const
//    {
//        const char* val;
//        int size;
//        DotsC_GetBinaryParameter(m_tid, m_param, index, val, size); return std::make_pair(val, static_cast<size_t>(size));
//    }
//    std::pair<boost::int64_t, const char*> GetHashedValue(int index) const
//    {
//        DotsC_Int64 hash;
//        const char* str;
//        DotsC_GetHashedIdParameter(m_tid, m_param, index, hash, str);
//        return std::make_pair(hash, str);
//    }
//private:
//    DotsC_TypeId m_tid;
//    DotsC_ParameterIndex m_param;
//};

//class EnumDescriptionKernel
//{
//public:
//    EnumDescriptionKernel(DotsC_TypeId tid)
//        :m_tid(tid)
//    {

//    }
//    const char* FileName() const {return DotsC_GetDouFilePath(m_tid);}
//    const char* Summary() const {return NULL;}
//    DotsC_TypeId GetTypeId() const {return m_tid;}
//    const char* GetName() const {return DotsC_GetTypeName(m_tid);}
//    DotsC_TypeId GetCheckSum() const {DotsC_TypeId cs; DotsC_GetEnumerationChecksum(m_tid, cs); return cs;}
//    int GetNumberOfValues() const {return DotsC_GetNumberOfEnumerationValues(m_tid);}
//    const char* GetValueName(DotsC_EnumerationValue val) const {return DotsC_GetEnumerationValueName(m_tid, val);}
//    int GetIndexOfValue(const std::string& valueName) const {return Safir::Dob::Typesystem::Internal::TypeUtilities::GetIndexOfEnumValue(this, valueName);}

//private:
//    DotsC_TypeId m_tid;
//};

//class MemberMappingDescriptionKernel
//{
//public:
//    DotsC_PropertyMappingKind GetMappingKind() const {return MappedToNull;}
//    std::pair<const ParameterDescriptionKernel*, int /*paramIndex*/> GetParameter() const {const ParameterDescriptionKernel* p=NULL; return std::make_pair(p, 0);} //if mapped to parameter. If paramIndex<0, whole array is.


//    //if mapped to member
//    int MemberReferenceDepth() const {return 0;}
//    std::pair<DotsC_MemberIndex, DotsC_ArrayIndex> GetMemberReference(int depth) const {return std::make_pair(0, 0);}
//};

//class PropertyMappingDescriptionKernel
//{
//public:
//    const char* FileName() const {return NULL;}//{return DotsC_GetDouFilePath(m_tid);}
//    const char* Summary() const {return NULL;}
//    const PropertyDescriptionKernel* GetProperty() const {return NULL;}
//    const ClassDescriptionKernel* GetClass() const {return NULL;}
//    const MemberMappingDescriptionKernel* GetMemberMapping(int propertyMemberIndex) const {return NULL;}
//};

//class CreateRoutineDescriptionKernel
//{
//public:
//    CreateRoutineDescriptionKernel() {}
//    const char* Summary() const {return NULL;}
//    const char* GetName() const {return NULL;}
//    int GetNumberOfInParameters() const {return 0;}
//    const MemberDescriptionKernel* GetInParameterMember(int /*index*/) const {return NULL;}
//    int GetNumberOfDefaultValues() const {return 0;}
//    const MemberDescriptionKernel* GetDefaultValueMember(int /*index*/) const {return NULL;}
//    std::pair<const ParameterDescriptionKernel*, int> GetDefaultValue(int /*index*/) const {const ParameterDescriptionKernel* pd=NULL; return std::make_pair(pd, 0);}
//};

//class ClassDescriptionKernel
//{
//public:
//    ClassDescriptionKernel(DotsC_TypeId tid)
//        :m_tid(tid)
//    {
//        DotsC_TypeId base=DotsC_GetParentType(m_tid);
//        if (base!=m_tid)
//        {
//            m_base=boost::make_shared<ClassDescriptionKernel>(base);
//        }

//        for (int i=GetNumberOfInheritedMembers(); i<DotsC_GetNumberOfMembers(m_tid); ++i)
//        {
//            m_members.push_back(MemberDescriptionKernel(m_tid, i));
//        }

//        for (int i=GetNumberOfInheritedParameters(); i<DotsC_GetNumberOfParameters(m_tid); ++i)
//        {
//            m_params.push_back(ParameterDescriptionKernel(m_tid, i));

//        }
//    }
//    const char* FileName() const {return DotsC_GetDouFilePath(m_tid);}
//    const char* Summary() const {return NULL;}
//    DotsC_TypeId GetTypeId() const {return m_tid;}
//    const char* GetName() const {return DotsC_GetTypeName(m_tid);}
//    const ClassDescriptionKernel* GetBaseClass() const {return NULL;}
//    int GetNumberOfDescendants() const {return 0;}
//    const ClassDescriptionKernel* GetDescendant(int index) const {return NULL;}

//    int GetNumberOfMembers() const {return GetNumberOfOwnMembers()+GetNumberOfInheritedMembers();}
//    int GetNumberOfOwnMembers() const {return static_cast<int>(m_members.size());}
//    int GetNumberOfInheritedMembers() const {return m_base ? m_base->GetNumberOfMembers() : 0;}
//    DotsC_MemberIndex GetMemberIndex(const std::string& memberName) const {return DotsC_GetMemberId(m_tid, memberName.c_str());}
//    const MemberDescriptionKernel* GetMember(DotsC_MemberIndex index) const
//    {
//        if (index<GetNumberOfInheritedMembers())
//        {
//            return m_base->GetMember(index);
//        }
//        return &m_members[index-GetNumberOfInheritedMembers()];
//    }

//    int GetNumberOfParameters() const {return GetNumberOfOwnParameters()+GetNumberOfInheritedParameters();}
//    int GetNumberOfOwnParameters() const {return static_cast<int>(m_params.size());}
//    int GetNumberOfInheritedParameters() const {return m_base ? m_base->GetNumberOfParameters() : 0;}
//    const ParameterDescriptionKernel* GetParameter(DotsC_ParameterIndex index) const
//    {
//        if (index<GetNumberOfInheritedParameters())
//        {
//            return m_base->GetParameter(index);
//        }
//        return &m_params[index-GetNumberOfInheritedParameters()];
//    }

//    void GetPropertyIds(std::set<DotsC_TypeId>& propertyIds) const
//    {
//        //TODO

//    }

//    const PropertyMappingDescriptionKernel* GetPropertyMapping(DotsC_TypeId propertyTypeId, bool & isInherited) const
//    {
//        //TODO
//        return NULL;
//    }
//    int GetNumberOfCreateRoutines() const {return 0;}
//    const CreateRoutineDescriptionKernel* GetCreateRoutine(int index) const {return NULL;}
//    int InitialSize() const {return DotsC_GetInitialSize(m_tid);}
//    int OwnSize() const {return DotsC_GetInitialSize(m_tid);}

//private:
//    DotsC_TypeId m_tid;
//    std::vector<MemberDescriptionKernel> m_members;
//    std::vector<ParameterDescriptionKernel> m_params;
//    boost::shared_ptr<ClassDescriptionKernel> m_base;
//};

//class TypeRepositoryKernel
//{
//public:
//    //Enmerations
//    const EnumDescriptionKernel* GetEnum(DotsC_TypeId typeId) const {return GetDescription<EnumDescriptionKernel>(typeId, DotsC_IsEnumeration, m_enums);}
//    size_t GetNumberOfEnums() const {return static_cast<size_t>(DotsC_NumberOfEnumerations());}
//    void GetAllEnumTypeIds(std::set<DotsC_TypeId>& typeIds) const {GetAllTypeIds(typeIds, DotsC_IsEnumeration);}

//    //properties
//    const PropertyDescriptionKernel* GetProperty(DotsC_TypeId typeId) const {return GetDescription<PropertyDescriptionKernel>(typeId, DotsC_IsProperty, m_properties);}
//    size_t GetNumberOfProperties() const {return static_cast<size_t>(DotsC_NumberOfProperties());}
//    void GetAllPropertyTypeIds(std::set<DotsC_TypeId>& typeIds) const {GetAllTypeIds(typeIds, DotsC_IsProperty);}

//    //classes
//    const ClassDescriptionKernel* GetClass(DotsC_TypeId typeId) const {return GetDescription<ClassDescriptionKernel>(typeId, DotsC_IsClass, m_classes);}
//    size_t GetNumberOfClasses() const {return static_cast<size_t>(DotsC_NumberOfClasses());}
//    void GetAllClassTypeIds(std::set<DotsC_TypeId>& typeIds) const {GetAllTypeIds(typeIds, DotsC_IsClass);}

//    //exceptions
//    const ExceptionDescriptionKernel* GetException(DotsC_TypeId typeId) const {return GetDescription<ExceptionDescriptionKernel>(typeId, DotsC_IsException, m_exceptions);}
//    size_t GetNumberOfExceptions() const {return static_cast<size_t>(DotsC_NumberOfExceptions());}
//    void GetAllExceptionTypeIds(std::set<DotsC_TypeId>& typeIds) const {GetAllTypeIds(typeIds, DotsC_IsException);}

//private:
//    mutable std::map<DotsC_TypeId, boost::shared_ptr<EnumDescriptionKernel> > m_enums;
//    mutable std::map<DotsC_TypeId, boost::shared_ptr<PropertyDescriptionKernel> > m_properties;
//    mutable std::map<DotsC_TypeId, boost::shared_ptr<ClassDescriptionKernel> > m_classes;
//    mutable std::map<DotsC_TypeId, boost::shared_ptr<ExceptionDescriptionKernel> > m_exceptions;

//    void GetAllTypeIds(std::set<DotsC_TypeId>& typeIds, boost::function<bool(DotsC_TypeId)> toBeInserted) const
//    {
//        std::vector<DotsC_TypeId> v;
//        v.resize(static_cast<size_t>(DotsC_NumberOfTypeIds()));
//        int size;
//        DotsC_GetAllTypeIds(&v[0], static_cast<DotsC_Int32>(v.size()), size);
//        for (std::vector<DotsC_TypeId>::const_iterator it=v.begin(); it!=v.end(); ++it)
//        {
//            if (toBeInserted(*it))
//            {
//                typeIds.insert(*it);
//            }
//        }
//    }

//    template <class DescrT>
//    const DescrT* GetDescription(DotsC_TypeId tid,
//                                 boost::function<bool(const DotsC_TypeId)> isDescr,
//                                 std::map<DotsC_TypeId, boost::shared_ptr<DescrT> > & descrMap) const
//    {
//        if (isDescr(tid))
//        {
//            typename std::map<DotsC_TypeId, boost::shared_ptr<DescrT> >::iterator it=descrMap.find(tid);
//            if (it!=descrMap.end())
//            {
//                return it->second.get();
//            }
//            else
//            {
//                boost::shared_ptr<DescrT> d=boost::make_shared<DescrT>(tid);
//                descrMap.insert(std::make_pair(tid, d));
//                return d.get();
//            }
//        }
//        else
//        {
//            return NULL;
//        }
//    }
//};

//namespace Safir{ namespace Dob{ namespace Typesystem{ namespace Internal{
//template<> struct TypeRepositoryTraits<TypeRepositoryKernel>
//{
//    typedef TypeRepositoryKernel RepositoryType;
//    typedef ClassDescriptionKernel ClassDescriptionType;
//    typedef MemberDescriptionKernel MemberDescriptionType;
//    typedef PropertyDescriptionKernel PropertyDescriptionType;
//    typedef ExceptionDescriptionKernel ExceptionDescriptionType;
//    typedef ParameterDescriptionKernel ParameterDescriptionType;
//    typedef EnumDescriptionKernel EnumDescriptionType;
//    typedef MemberMappingDescriptionKernel MemberMappingDescriptionType;
//    typedef PropertyMappingDescriptionKernel PropertyMappingDescriptionType;
//    typedef CreateRoutineDescriptionKernel CreateRoutineDescriptionType;
//};
//}}}}
////===================================================================================================
