/******************************************************************************
*
* Copyright Saab AB, 2006-2013 (http://safir.sourceforge.net)
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

#ifndef __DOTS_SERIALIZATION_H__
#define __DOTS_SERIALIZATION_H__

#include <boost/shared_ptr.hpp>
#include <Safir/Dob/Typesystem/Defs.h>
#include <Safir/Dob/Typesystem/Exceptions.h>

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
    //forward declarations
    class Object;

    /** A smart pointer to an Object. */
    typedef boost::shared_ptr<Object> ObjectPtr;

    /**
    * Functions for serializing objects to binary and XML forms.
    */
    class DOTS_CPP_API Serialization
    {
    public:
        /**
         * Serialize an object to XML.
         *
         * @param [in] object - The object to serialize
         * @return The xml serialization
         * @exception IllegalValueException - There is something wrong with the object.
         */
        static const std::wstring ToXml(const Dob::Typesystem::ObjectPtr object);

        /**
         * Deserialize an XML serialization.
         *
         * This method creates a new object from a given xml serialization.
         * It uses the ObjectFactory to accomplish this.
         *
         * @param [in] xml - The xml to convert.
         * @return A boost::shared_ptr to the new object
         * @exception IllegalValueException If there is something wrong with the XML or if the type
         *                                  represented by the serialization isn't found
         *                                  in the ObjectFactory.
         */
        static Dob::Typesystem::ObjectPtr ToObject(const std::wstring & xml);

        /**
         * Convert a binary serialization to XML.
         *
         * @param [in] bin - the binary serialization to convert to xml.
         * @return The xml of the binary serialization.
         */
        static const std::wstring ToXml(const BinarySerialization & bin);

        /**
         * Convert a blob to XML.
         *
         * @param [in] blob - the blob to convert to xml.
         * @return The xml of the blob.
         */
        static const std::wstring ToXml(const char * const blob);

        /**
         * Serialize an object to binary form.
         *
         * The serialization is put into a variable of type BinarySerialization, which
         * is of type std::vector<char>. If you need to get hold of a "raw" C-pointer to the data
         * use &binary[0]. See Effective STL Item 16 for more info.
         *
         * @param [in] object - The object to serialize
         * @param [out] binary - The destination of the serialization
         * @exception IllegalValueException - There is something wrong with the object.
         */
        static void ToBinary(const Dob::Typesystem::ObjectPtr object, Dob::Typesystem::BinarySerialization & binary);


        /**
         * Deserialize a binary serialization and create an object.
         *
         * It uses the ObjectFactory to accomplish this.
         * If you have a char * that you want to deserialize the easiest way is to
         * pass it to the ObjectFactory instead.
         *
         * @param [in] binary - The binary serialization to deserialize.
         * @return A boost::shared_ptr to the new object
         * @exception IllegalValueException If the type represented by the serialization isn't found
         *                                   in the ObjectFactory.
         */
        static Dob::Typesystem::ObjectPtr ToObject(const Dob::Typesystem::BinarySerialization & binary);

    };
}
}
}


#endif

