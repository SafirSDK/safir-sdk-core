/******************************************************************************
*
* Copyright Saab AB, 2013 (http://safir.sourceforge.net)
*
* Created by: Lars Hagström / lars.hagstrom@consoden.se
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
#pragma once

#include <Safir/Dob/Internal/SystemPictureDefs.h>
#include <functional>
#include <memory>
#include <ostream>
#include <string>

//Forward declare some asio stuff.
namespace boost
{
namespace asio
{
    class io_service;
}
}


namespace Safir
{
namespace Dob
{
namespace Internal
{
namespace SP
{

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4275)
#pragma warning (disable: 4251)
#endif


    /** 
     * This is an interface to the System State information produced by System Picture.
     *
     * The object contains all nodes that System Picture currently thinks is part of the 
     * system. Nodes that are being excluded from the system will be marked as dead for 
     * _a while_ (a few minutes), before they are removed completely.
     *
     * Use functionality in SystemPicture class to get an instance to one
     * of these objects.
     */
    class DISTRIBUTION_SYSTEM_PICTURE_API SystemState
    {
    public:
        /** 
         * Default constructor.
         *
         * This constructor will create an "invalid" object, where the only guarantee
         * is that Size() will return 0. All other member functions yield undefined behavior.
         */
        SystemState();

        /**
         * Get the node id of the elected node.
         */
        int64_t ElectedId() const;

        /**
         * Get the id of the current election. Each election has a unique identifier, so 
         * this id can be used to determine if a new election has taken place.
         */
        int64_t ElectionId() const;
        
        /** 
         * Get the number of nodes in the system state. 
         *
         * The methods below expect an index that is in the range 0 to Size() - 1. Indexing 
         * outside that range causes undefined behavior.
         */
        int Size() const;

        /** Get the name of the node. */
        const std::string& Name(const int index) const;

        /** Get the node id of the node. */
        int64_t Id(const int index) const;

        /** Get the node type id of the node. */
        int64_t NodeTypeId(const int index) const;

        /** Get the address of the control channel of this node. */
        const std::string& ControlAddress(const int index) const;

        /** Get the address of the data channel of this node. */
        const std::string& DataAddress(const int index) const;

        /** 
         * Check whether the node has been declared as dead (or excluded) recently.
         *
         * After a while dead nodes will be removed from the list of nodes.
         */
        bool IsDead(const int index) const;

        /** Print the contents of the object to the output stream. */
        void Print(std::wostream& out) const;

    private:
        friend class SystemStateCreator;

        void CheckValid() const;

        class Impl;

        explicit SystemState(std::shared_ptr<Impl>  impl)
            : m_impl(std::move(impl)) {}

        std::shared_ptr<Impl> m_impl;

    };

#ifdef _MSC_VER
#pragma warning (pop)
#endif

    /**
     * Interface to an object that allows or subscriptions to system state objects.
     *
     * This interface is not intended for users to implement. Instead use the SystemPicture 
     * class to obtain an instance of a class that implements this interface.
     */
    class SystemStateSubscriber
    {
    public:
        typedef SystemState DataWrapper;

        /** Start a subscription to system states. */
        virtual void Start(boost::asio::io_service& ioService,
                           const std::function<void (const SystemState& data)>& dataCallback) = 0;

        /** Stop the subscription. */
        virtual void Stop() = 0;
    };


    /** Output operator for SystemStates. */
    static inline std::wostream& operator<<(std::wostream& out, const SystemState& state)
    {
        state.Print(out);
        return out;
    }
}
}
}
}

