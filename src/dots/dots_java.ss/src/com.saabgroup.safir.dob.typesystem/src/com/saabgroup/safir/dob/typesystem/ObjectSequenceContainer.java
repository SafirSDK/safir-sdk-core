package com.saabgroup.safir.dob.typesystem;

public class ObjectSequenceContainer
extends GenericObjectSequenceContainer<com.saabgroup.safir.dob.typesystem.Object>
implements Cloneable {
	
	ObjectSequenceContainer(ObjectSequenceContainer other) {
		super(other);
	}
	
	/**
     * @see com.saabgroup.safir.dob.typesystem.ValueContainer#clone()
     */
    @Override
    public ObjectSequenceContainer clone() {
        return new ObjectSequenceContainer(this);
    }

}
