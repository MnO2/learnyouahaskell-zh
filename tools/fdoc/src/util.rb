class String
    def head
        self[0..0]
    end
    def tail
        self[1..self.length]
    end

    def getline
        line, rest=self.split(/\n/, 2)

        self.replace(rest) 
        self.replace('') if rest.nil?
        return line
    end
end

