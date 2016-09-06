//TODO header
package com.saabgroup.safir.dob.typesystem;

import java.util.Collection;
import java.util.Map;
import java.util.Set;

public class ObjectDictionaryContainer<K, C extends ObjectContainerImpl<O>, O extends com.saabgroup.safir.dob.typesystem.Object>
    extends DictionaryContainer<K,C>
{
    public ObjectDictionaryContainer(final Class containerClass) {
        super();
        m_containerClass = (Class<C>)containerClass;
    }

    public C putObj(K key, O object) {
        m_isChanged=true;
        C container;
        try {
            container = m_containerClass.newInstance();
        }
        catch (InstantiationException | IllegalAccessException e) {
            throw new SoftwareViolationException("Internal error in ObjectDictionaryContainer: " +
                                                 "Failed to instantiate container.");
        }
        container.setObj(object);

        return m_values.put(key, container);
    }

    private final Class<C> m_containerClass;
}
